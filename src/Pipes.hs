{-# LANGUAGE CPP                   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE Trustworthy           #-}

{-| This module is the recommended entry point to the @pipes@ library.

    Read "Pipes.Tutorial" if you want a tutorial explaining how to use this
    library.
-}

module Pipes (
    -- * The Proxy Monad Transformer
      Proxy
    , X
    , Effect
    , Effect'
    , runEffect

    -- ** Producers
    -- $producers
    , Producer
    , Producer'
    , yield
    , for
    , (~>)
    , (<~)

    -- ** Consumers
    -- $consumers
    , Consumer
    , Consumer'
    , await
    , (>~)
    , (~<)

    -- ** Pipes
    -- $pipes
    , Pipe
    , cat
    , (>->)
    , (<-<)

    -- * ListT
    , ListT(..)
    , runListT
    , evalListT
    , Enumerable(..)

    -- * Utilities
    , next
    , each
    , every
    , discard

    -- * Re-exports
    -- $reexports
    , module Control.Monad
    , module Control.Monad.IO.Class
    , module Control.Monad.Trans.Class
    , module Control.Monad.Morph
    , Foldable
    ) where

import Control.Monad (void, MonadPlus(mzero, mplus))
import Control.Monad.Catch (MonadThrow(..), MonadCatch(..))
import Control.Monad.Except (MonadError(..))
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Identity (IdentityT(runIdentityT))
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Control.Monad.Writer (MonadWriter(..))
import Control.Monad.Zip (MonadZip(..))
import Pipes.Core
import Pipes.Internal (Proxy(..))
import Pipes.Lift (writerP, execWriterP)
import qualified Data.Foldable as F
import qualified Control.Monad as M

#if MIN_VERSION_base(4,8,0)
import Control.Applicative (Alternative(..))
#else
import Control.Applicative
import Data.Foldable (Foldable)
import Data.Traversable (Traversable(..))
#endif
import Data.Semigroup

-- Re-exports
import Control.Monad.Morph (MFunctor(hoist), MMonad(embed))

infixl 4 <~
infixr 4 ~>
infixl 5 ~<
infixr 5 >~
infixl 7 >->
infixr 7 <-<

{- $producers
    Use 'yield' to produce output and ('~>') \/ 'for' to substitute 'yield's.

    'yield' and ('~>') obey the 'Control.Category.Category' laws:

@
\-\- Substituting \'yield\' with \'f\' gives \'f\'
'yield' '~>' f = f

\-\- Substituting every \'yield\' with another \'yield\' does nothing
f '~>' 'yield' = f

\-\- \'yield\' substitution is associative
(f '~>' g) '~>' h = f '~>' (g '~>' h)
@

    These are equivalent to the following \"for loop laws\":

@
\-\- Looping over a single yield simplifies to function application
'for' ('yield' x) f = f x

\-\- Re-yielding every element of a stream returns the original stream
'for' s 'yield' = s

\-\- Nested for loops can become a sequential 'for' loops if the inner loop
\-\- body ignores the outer loop variable
'for' s (\\a -\> 'for' (f a) g) = 'for' ('for' s f) g = 'for' s (f '~>' g)
@

-}

{-| Produce a value

@
'yield' :: 'Monad' m => a -> 'Producer' a m ()
'yield' :: 'Monad' m => a -> 'Pipe'   x a m ()
@
-}
yield :: Functor m => a -> Proxy x' x () a m ()
yield = respond
{-# INLINABLE [1] yield #-}

{-| @(for p body)@ loops over @p@ replacing each 'yield' with @body@.

@
'for' :: 'Functor' m => 'Producer' b m r -> (b -> 'Effect'       m ()) -> 'Effect'       m r
'for' :: 'Functor' m => 'Producer' b m r -> (b -> 'Producer'   c m ()) -> 'Producer'   c m r
'for' :: 'Functor' m => 'Pipe'   x b m r -> (b -> 'Consumer' x   m ()) -> 'Consumer' x   m r
'for' :: 'Functor' m => 'Pipe'   x b m r -> (b -> 'Pipe'     x c m ()) -> 'Pipe'     x c m r
@

    The following diagrams show the flow of information:

@
                              .--->   b
                             /        |
   +-----------+            /   +-----|-----+                 +---------------+
   |           |           /    |     v     |                 |               |
   |           |          /     |           |                 |               |
x ==>    p    ==> b   ---'   x ==>   body  ==> c     =     x ==> 'for' p body  ==> c
   |           |                |           |                 |               |
   |     |     |                |     |     |                 |       |       |
   +-----|-----+                +-----|-----+                 +-------|-------+
         v                            v                               v
         r                            ()                              r
@

    For a more complete diagram including bidirectional flow, see "Pipes.Core#respond-diagram".
-}
for :: Functor m
    =>       Proxy x' x b' b m a'
    -- ^
    -> (b -> Proxy x' x c' c m b')
    -- ^
    ->       Proxy x' x c' c m a'
for = (//>)
-- There are a number of useful rewrites which can be performed on various uses
-- of this combinator; to ensure that they fire we defer inlining until quite
-- late.
{-# INLINABLE [0] for #-}

{-# RULES
    "for (for p f) g" forall p f g . for (for p f) g = for p (\a -> for (f a) g)

  ; "for p yield" forall p . for p yield = p

  ; "for (yield x) f" forall x f . for (yield x) f = f x

  ; "for cat f" forall f .
        for cat f =
            let go = do
                    x <- await
                    f x
                    go
            in  go

  ; "f >~ (g >~ p)" forall f g p . f >~ (g >~ p) = (f >~ g) >~ p

  ; "await >~ p" forall p . await >~ p = p

  ; "p >~ await" forall p . p >~ await = p

  ; "m >~ cat" forall m .
        m >~ cat =
            let go = do
                    x <- m
                    yield x
                    go
            in  go

  ; "p1 >-> (p2 >-> p3)" forall p1 p2 p3 .
        p1 >-> (p2 >-> p3) = (p1 >-> p2) >-> p3

  ; "p >-> cat" forall p . p >-> cat = p

  ; "cat >-> p" forall p . cat >-> p = p

  #-}

{-| Compose loop bodies

@
('~>') :: 'Functor' m => (a -> 'Producer' b m r) -> (b -> 'Effect'       m ()) -> (a -> 'Effect'       m r)
('~>') :: 'Functor' m => (a -> 'Producer' b m r) -> (b -> 'Producer'   c m ()) -> (a -> 'Producer'   c m r)
('~>') :: 'Functor' m => (a -> 'Pipe'   x b m r) -> (b -> 'Consumer' x   m ()) -> (a -> 'Consumer' x   m r)
('~>') :: 'Functor' m => (a -> 'Pipe'   x b m r) -> (b -> 'Pipe'     x c m ()) -> (a -> 'Pipe'     x c m r)
@

    The following diagrams show the flow of information:

@
         a                    .--->   b                              a
         |                   /        |                              |
   +-----|-----+            /   +-----|-----+                 +------|------+
   |     v     |           /    |     v     |                 |      v      |
   |           |          /     |           |                 |             |
x ==>    f    ==> b   ---'   x ==>    g    ==> c     =     x ==>   f '~>' g  ==> c
   |           |                |           |                 |             |
   |     |     |                |     |     |                 |      |      |
   +-----|-----+                +-----|-----+                 +------|------+
         v                            v                              v
         r                            ()                             r
@

    For a more complete diagram including bidirectional flow, see "Pipes.Core#respond-diagram".
-}
(~>)
    :: Functor m
    => (a -> Proxy x' x b' b m a')
    -- ^
    -> (b -> Proxy x' x c' c m b')
    -- ^
    -> (a -> Proxy x' x c' c m a')
(~>) = (/>/)
{-# INLINABLE (~>) #-}

-- | ('~>') with the arguments flipped
(<~)
    :: Functor m
    => (b -> Proxy x' x c' c m b')
    -- ^
    -> (a -> Proxy x' x b' b m a')
    -- ^
    -> (a -> Proxy x' x c' c m a')
g <~ f = f ~> g
{-# INLINABLE (<~) #-}

{- $consumers
    Use 'await' to request input and ('>~') to substitute 'await's.

    'await' and ('>~') obey the 'Control.Category.Category' laws:

@
\-\- Substituting every \'await\' with another \'await\' does nothing
'await' '>~' f = f

\-\- Substituting \'await\' with \'f\' gives \'f\'
f '>~' 'await' = f

\-\- \'await\' substitution is associative
(f '>~' g) '>~' h = f '>~' (g '>~' h)
@

-}

{-| Consume a value

@
'await' :: 'Functor' m => 'Pipe' a y m a
@
-}
await :: Functor m => Consumer' a m a
await = request ()
{-# INLINABLE [1] await #-}

{-| @(draw >~ p)@ loops over @p@ replacing each 'await' with @draw@

@
('>~') :: 'Functor' m => 'Effect'       m b -> 'Consumer' b   m c -> 'Effect'       m c
('>~') :: 'Functor' m => 'Consumer' a   m b -> 'Consumer' b   m c -> 'Consumer' a   m c
('>~') :: 'Functor' m => 'Producer'   y m b -> 'Pipe'     b y m c -> 'Producer'   y m c
('>~') :: 'Functor' m => 'Pipe'     a y m b -> 'Pipe'     b y m c -> 'Pipe'     a y m c
@

    The following diagrams show the flow of information:

@
   +-----------+                 +-----------+                 +-------------+
   |           |                 |           |                 |             |
   |           |                 |           |                 |             |
a ==>    f    ==> y   .--->   b ==>    g    ==> y     =     a ==>   f '>~' g  ==> y
   |           |     /           |           |                 |             |
   |     |     |    /            |     |     |                 |      |      |
   +-----|-----+   /             +-----|-----+                 +------|------+
         v        /                    v                              v
         b   ----'                     c                              c
@

    For a more complete diagram including bidirectional flow, see "Pipes.Core#request-diagram".
-}
(>~)
    :: Functor m
    => Proxy a' a y' y m b
    -- ^
    -> Proxy () b y' y m c
    -- ^
    -> Proxy a' a y' y m c
p1 >~ p2 = (\() -> p1) >\\ p2
{-# INLINABLE [1] (>~) #-}

-- | ('>~') with the arguments flipped
(~<)
    :: Functor m
    => Proxy () b y' y m c
    -- ^
    -> Proxy a' a y' y m b
    -- ^
    -> Proxy a' a y' y m c
p2 ~< p1 = p1 >~ p2
{-# INLINABLE (~<) #-}

{- $pipes
    Use 'await' and 'yield' to build 'Pipe's and ('>->') to connect 'Pipe's.

    'cat' and ('>->') obey the 'Control.Category.Category' laws:

@
\-\- Useless use of cat
'cat' '>->' f = f

\-\- Redirecting output to cat does nothing
f '>->' 'cat' = f

\-\- The pipe operator is associative
(f '>->' g) '>->' h = f '>->' (g '>->' h)
@

-}

-- | The identity 'Pipe', analogous to the Unix @cat@ program
cat :: Functor m => Pipe a a m r
cat = pull ()
{-# INLINABLE [1] cat #-}

{-| 'Pipe' composition, analogous to the Unix pipe operator

@
('>->') :: 'Functor' m => 'Producer' b m r -> 'Consumer' b   m r -> 'Effect'       m r
('>->') :: 'Functor' m => 'Producer' b m r -> 'Pipe'     b c m r -> 'Producer'   c m r
('>->') :: 'Functor' m => 'Pipe'   a b m r -> 'Consumer' b   m r -> 'Consumer' a   m r
('>->') :: 'Functor' m => 'Pipe'   a b m r -> 'Pipe'     b c m r -> 'Pipe'     a c m r
@

    The following diagrams show the flow of information:

@
   +-----------+     +-----------+                 +-------------+
   |           |     |           |                 |             |
   |           |     |           |                 |             |
a ==>    f    ==> b ==>    g    ==> c     =     a ==>  f '>->' g  ==> c
   |           |     |           |                 |             |
   |     |     |     |     |     |                 |      |      |
   +-----|-----+     +-----|-----+                 +------|------+
         v                 v                              v
         r                 r                              r
@

    For a more complete diagram including bidirectional flow, see "Pipes.Core#pull-diagram".
-}
(>->)
    :: Functor m
    => Proxy a' a () b m r
    -- ^
    -> Proxy () b c' c m r
    -- ^
    -> Proxy a' a c' c m r
p1 >-> p2 = (\() -> p1) +>> p2
{-# INLINABLE [1] (>->) #-}

{-| The list monad transformer, which extends a monad with non-determinism

    The type variables signify:

      * @m@ - The base monad
      * @a@ - The values that the computation 'yield's throughout its execution

    For basic construction and composition of 'ListT' computations, much can be
    accomplished using common typeclass methods.

      * 'return' corresponds to 'yield', yielding a single value.
      * ('>>=') corresponds to 'for', calling the second computation once
        for each time the first computation 'yield's.
      * 'mempty' neither 'yield's any values nor produces any effects in the
        base monad.
      * ('<>') sequences two computations, 'yield'ing all the values of the
        first followed by all the values of the second.
      * 'lift' converts an action in the base monad into a ListT computation
        which performs the action and 'yield's a single value.

    'ListT' is a newtype wrapper for 'Producer'. You will likely need to use
    'Select' and 'enumerate' to convert back and forth between these two types
    to take advantage of all the 'Producer'-related utilities that
    "Pipes.Prelude" has to offer.

      * To lift a plain list into a 'ListT' computation, first apply 'each'
        to turn the list into a 'Producer'. Then apply the 'Select'
        constructor to convert from 'Producer' to 'ListT'.
      * For other ways to construct 'ListT' computations, see the
        “Producers” section in "Pipes.Prelude" to build 'Producer's.
        These can then be converted to 'ListT' using 'Select'.
      * To aggregate the values from a 'ListT' computation (for example,
        to compute the sum of a 'ListT' of numbers), first apply
        'enumerate' to obtain a 'Producer'. Then see the “Folds”
        section in "Pipes.Prelude" to proceed.
-}
newtype ListT m a = Select { enumerate :: Producer a m () }

instance Functor m => Functor (ListT m) where
    fmap f p = Select (for (enumerate p) (\a -> yield (f a)))
    {-# INLINE fmap #-}

instance Functor m => Applicative (ListT m) where
    pure a = Select (yield a)
    {-# INLINE pure #-}
    mf <*> mx = Select (
        for (enumerate mf) (\f ->
        for (enumerate mx) (\x ->
        yield (f x) ) ) )

instance Monad m => Monad (ListT m) where
    return   = pure
    {-# INLINE return #-}
    m >>= f  = Select (for (enumerate m) (\a -> enumerate (f a)))
    {-# INLINE (>>=) #-}
#if !MIN_VERSION_base(4,13,0)
    fail _   = mzero
    {-# INLINE fail #-}
#endif

instance Monad m => MonadFail (ListT m) where
    fail _ = mzero
    {-# INLINE fail #-}

instance Foldable m => Foldable (ListT m) where
    foldMap f = go . enumerate
      where
        go p = case p of
            Request v _  -> closed v
            Respond a fu -> f a `mappend` go (fu ())
            M       m    -> F.foldMap go m
            Pure    _    -> mempty
    {-# INLINE foldMap #-}

instance (Functor m, Traversable m) => Traversable (ListT m) where
    traverse k (Select p) = fmap Select (traverse_ p)
      where
        traverse_ (Request v _ ) = closed v
        traverse_ (Respond a fu) = _Respond <$> k a <*> traverse_ (fu ())
          where
            _Respond a_ a' = Respond a_ (\_ -> a')
        traverse_ (M       m   ) = fmap M (traverse traverse_ m)
        traverse_ (Pure     r  ) = pure (Pure r)

instance MonadTrans ListT where
    lift m = Select (do
        a <- lift m
        yield a )

instance (MonadIO m) => MonadIO (ListT m) where
    liftIO m = lift (liftIO m)
    {-# INLINE liftIO #-}

instance (Functor m) => Alternative (ListT m) where
    empty = Select (return ())
    {-# INLINE empty #-}
    p1 <|> p2 = Select (do
        enumerate p1
        enumerate p2 )

instance (Monad m) => MonadPlus (ListT m) where
    mzero = empty
    {-# INLINE mzero #-}
    mplus = (<|>)
    {-# INLINE mplus #-}

instance MFunctor ListT where
    hoist morph = Select . hoist morph . enumerate
    {-# INLINE hoist #-}

instance MMonad ListT where
    embed f (Select p0) = Select (loop p0)
      where
        loop (Request a' fa ) = Request a' (\a  -> loop (fa  a ))
        loop (Respond b  fb') = Respond b  (\b' -> loop (fb' b'))
        loop (M          m  ) = for (enumerate (fmap loop (f m))) id
        loop (Pure    r     ) = Pure r
    {-# INLINE embed #-}

instance (Functor m) => Semigroup (ListT m a) where
    (<>) = (<|>)
    {-# INLINE (<>) #-}

instance (Functor m) => Monoid (ListT m a) where
    mempty = empty
    {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<|>)
    {-# INLINE mappend #-}
#endif

instance (MonadState s m) => MonadState s (ListT m) where
    get     = lift  get
    {-# INLINE get #-}

    put   s = lift (put   s)
    {-# INLINE put #-}

    state f = lift (state f)
    {-# INLINE state #-}

instance (MonadWriter w m) => MonadWriter w (ListT m) where
    writer = lift . writer
    {-# INLINE writer #-}

    tell w = lift (tell w)
    {-# INLINE tell #-}

    listen l = Select (go (enumerate l) mempty)
      where
        go p w = case p of
            Request a' fa  -> Request a' (\a  -> go (fa  a ) w)
            Respond b  fb' -> Respond (b, w)  (\b' -> go (fb' b') w)
            M          m   -> M (do
                (p', w') <- listen m
                return (go p' $! mappend w w') )
            Pure    r      -> Pure r

    pass l = Select (go (enumerate l) mempty)
      where
        go p w = case p of
            Request  a'     fa  -> Request a' (\a  -> go (fa  a ) w)
            Respond (b, f)  fb' -> M (pass (return
                (Respond b (\b' -> go (fb' b') (f w)), \_ -> f w) ))
            M               m   -> M (do
                (p', w') <- listen m
                return (go p' $! mappend w w') )
            Pure     r          -> Pure r

instance (MonadReader i m) => MonadReader i (ListT m) where
    ask = lift ask
    {-# INLINE ask #-}

    local f l = Select (local f (enumerate l))
    {-# INLINE local #-}

    reader f = lift (reader f)
    {-# INLINE reader #-}

instance (MonadError e m) => MonadError e (ListT m) where
    throwError e = lift (throwError e)
    {-# INLINE throwError #-}

    catchError l k = Select (catchError (enumerate l) (\e -> enumerate (k e)))
    {-# INLINE catchError #-}

instance MonadThrow m => MonadThrow (ListT m) where
    throwM = Select . throwM
    {-# INLINE throwM #-}

instance MonadCatch m => MonadCatch (ListT m) where
    catch l k = Select (Control.Monad.Catch.catch (enumerate l) (\e -> enumerate (k e)))
    {-# INLINE catch #-}

instance Monad m => MonadZip (ListT m) where
    mzipWith f = go
      where
        go xs ys = Select $ do
            xres <- lift $ next (enumerate xs)
            case xres of
                Left r -> return r
                Right (x, xnext) -> do
                    yres <- lift $ next (enumerate ys)
                    case yres of
                        Left r -> return r
                        Right (y, ynext) -> do
                            yield (f x y)
                            enumerate (go (Select xnext) (Select ynext))

-- | Run a self-contained `ListT` computation
runListT :: Monad m => ListT m a -> m ()
runListT l = runEffect (enumerate (l >> mzero))
{-# INLINABLE runListT #-}

-- | Run a self-contained `ListT` computation and accumulate the produced values as a list in the underlying monad
evalListT :: Monad m => ListT m a -> m [a]
evalListT listT =
      runEffect
  $   execWriterP
  $   (writerP $ (\() -> ((), [])) <$> enumerate listT)
  >-> (M.forever $ pass ((\element -> ((), (element :))) <$> await))

{-| 'Enumerable' generalizes 'Data.Foldable.Foldable', converting effectful
    containers to 'ListT's.

    Instances of 'Enumerable' must satisfy these two laws:

> toListT (return r) = return r
>
> toListT $ do x <- m  =  do x <- toListT m
>              f x           toListT (f x)

    In other words, 'toListT' is monad morphism.
-}
class Enumerable t where
    toListT :: Monad m => t m a -> ListT m a

instance Enumerable ListT where
    toListT = id

instance Enumerable IdentityT where
    toListT m = Select $ do
        a <- lift $ runIdentityT m
        yield a

instance Enumerable MaybeT where
    toListT m = Select $ do
        x <- lift $ runMaybeT m
        case x of
            Nothing -> return ()
            Just a  -> yield a

instance Enumerable (ExceptT e) where
    toListT m = Select $ do
        x <- lift $ runExceptT m
        case x of
            Left  _ -> return ()
            Right a -> yield a

{-| Consume the first value from a 'Producer'

    'next' either fails with a 'Left' if the 'Producer' terminates or succeeds
    with a 'Right' providing the next value and the remainder of the 'Producer'.
-}
next :: Monad m => Producer a m r -> m (Either r (a, Producer a m r))
next = go
  where
    go p = case p of
        Request v _  -> closed v
        Respond a fu -> return (Right (a, fu ()))
        M         m  -> m >>= go
        Pure    r    -> return (Left r)
{-# INLINABLE next #-}

{-| Convert a 'F.Foldable' to a 'Producer'

@
'each' :: ('Functor' m, 'Foldable' f) => f a -> 'Producer' a m ()
@
-}
each :: (Functor m, Foldable f) => f a -> Proxy x' x () a m ()
each = F.foldr (\a p -> yield a >> p) (return ())
{-# INLINABLE each #-}
{-  The above code is the same as:

> each = Data.Foldable.mapM_ yield

    ... except writing it directly in terms of `Data.Foldable.foldr` improves
    build/foldr fusion
-}

{-| Convert an 'Enumerable' to a 'Producer'

@
'every' :: ('Monad' m, 'Enumerable' t) => t m a -> 'Producer' a m ()
@
-}
every :: (Monad m, Enumerable t) => t m a -> Proxy x' x () a m ()
every it = discard >\\ enumerate (toListT it)
{-# INLINABLE every #-}

-- | Discards a value
discard :: Monad m => a -> m ()
discard _ = return ()
{-# INLINABLE discard #-}

-- | ('>->') with the arguments flipped
(<-<)
    :: Functor m
    => Proxy () b c' c m r
    -- ^
    -> Proxy a' a () b m r
    -- ^
    -> Proxy a' a c' c m r
p2 <-< p1 = p1 >-> p2
{-# INLINABLE (<-<) #-}

{- $reexports
    "Control.Monad" re-exports 'void'

    "Control.Monad.IO.Class" re-exports 'MonadIO'.

    "Control.Monad.Trans.Class" re-exports 'MonadTrans'.

    "Control.Monad.Morph" re-exports 'MFunctor'.

    "Data.Foldable" re-exports 'Foldable' (the class name only).
-}
