{-| Many actions in base monad transformers cannot be automatically
    'Control.Monad.Trans.Class.lift'ed.  These functions lift these remaining
    actions so that they work in the 'Proxy' monad transformer.

    See the mini-tutorial at the bottom of this module for example code and
    typical use cases where this module will come in handy.
-}

module Pipes.Lift (
    -- * Utilities
      distribute

    -- * ErrorT
    , errorP
    , runErrorP
    , catchError
    , liftCatchError

    -- * MaybeT
    , maybeP
    , runMaybeP

    -- * ReaderT
    , readerP
    , runReaderP

    -- * StateT
    , stateP
    , runStateP
    , evalStateP
    , execStateP

    -- * WriterT
    -- $writert
    , writerP
    , runWriterP
    , execWriterP

    -- * RWST
    , rwsP
    , runRWSP
    , evalRWSP
    , execRWSP

    -- * Tutorial
    -- $tutorial
    ) where

import Control.Monad.Trans.Class (lift, MonadTrans(..))
import qualified Control.Monad.Trans.Error as E
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Strict as W
import qualified Control.Monad.Trans.RWS.Strict as RWS
import Data.Monoid (Monoid)
import Pipes.Internal (Proxy(..), fixm, unsafeHoist)
import Control.Monad.Morph (hoist, MFunctor(..))
import Pipes.Core (runEffect, request, respond, (//>), (>\\))

-- | Distribute 'Proxy' over a monad transformer
distribute
    ::  ( Monad m
        , MonadTrans t
        , MFunctor t
        , Monad (t m)
        , Monad (t (Proxy a' a b' b m))
        )
    => Proxy a' a b' b (t m) r
    -- ^ 
    -> t (Proxy a' a b' b m) r
    -- ^ 
distribute p =  runEffect $ request' >\\ unsafeHoist (hoist lift) p //> respond'
  where
    request' = lift . lift . request
    respond' = lift . lift . respond
{-# INLINABLE distribute #-}

-- | Wrap the base monad in 'E.ErrorT'
errorP
    :: (Monad m, E.Error e)
    => Proxy a' a b' b m (Either e r)
    -> Proxy a' a b' b (E.ErrorT e m) r
errorP p = do
    x <- unsafeHoist lift p
    lift $ E.ErrorT (return x)
{-# INLINABLE errorP #-}

-- | Run 'E.ErrorT' in the base monad
runErrorP
    :: (Monad m, E.Error e)
    => Proxy a' a b' b (E.ErrorT e m) r
    -> Proxy a' a b' b m (Either e r)
runErrorP    = E.runErrorT . distribute 
{-# INLINABLE runErrorP #-}

-- | Catch an error in the base monad
catchError
    :: (Monad m, E.Error e) 
    => Proxy a' a b' b (E.ErrorT e m) r
    -- ^
    -> (e -> Proxy a' a b' b (E.ErrorT e m) r)
    -- ^
    -> Proxy a' a b' b (E.ErrorT e m) r
catchError e h = errorP . E.runErrorT $ 
    E.catchError (distribute e) (distribute . h)
{-# INLINABLE catchError #-}

-- | Catch an error using a catch function for the base monad
liftCatchError
    :: Monad m
    => (   m (Proxy a' a b' b m r)
        -> (e -> m (Proxy a' a b' b m r))
        -> m (Proxy a' a b' b m r) )
    -- ^
    ->    (Proxy a' a b' b m r
        -> (e -> Proxy a' a b' b m r)
        -> Proxy a' a b' b m r)
    -- ^
liftCatchError c p0 f = go p0
  where
    go p = case p of
        Pure    r      -> Pure r
        M          m   -> M ((do
            p' <- m
            return (go p') ) `c` (\e -> return (f e)) )
        _              -> fixm go p
{-# INLINABLE liftCatchError #-}

-- | Wrap the base monad in 'M.MaybeT'
maybeP
    :: Monad m
    => Proxy a' a b' b m (Maybe r) -> Proxy a' a b' b (M.MaybeT m) r
maybeP p = do
    x <- unsafeHoist lift p
    lift $ M.MaybeT (return x)
{-# INLINABLE maybeP #-}

-- | Run 'M.MaybeT' in the base monad
runMaybeP
    :: Monad m
    => Proxy a' a b' b (M.MaybeT m) r
    -> Proxy a' a b' b m (Maybe r)
runMaybeP p = M.runMaybeT $ distribute p
{-# INLINABLE runMaybeP #-}

-- | Wrap the base monad in 'R.ReaderT'
readerP
    :: Monad m
    => (i -> Proxy a' a b' b m r) -> Proxy a' a b' b (R.ReaderT i m) r
readerP k = do
    i <- lift R.ask
    unsafeHoist lift (k i)
{-# INLINABLE readerP #-}

-- | Run 'R.ReaderT' in the base monad
runReaderP
    :: Monad m
    => i
    -> Proxy a' a b' b (R.ReaderT i m) r
    -> Proxy a' a b' b m r
runReaderP r p = (`R.runReaderT` r) $ distribute p
{-# INLINABLE runReaderP #-}

-- | Wrap the base monad in 'S.StateT'
stateP
    :: Monad m
    => (s -> Proxy a' a b' b m (r, s)) -> Proxy a' a b' b (S.StateT s m) r
stateP k = do
    s <- lift S.get
    (r, s') <- unsafeHoist lift (k s)
    lift (S.put s')
    return r
{-# INLINABLE stateP #-}

-- | Run 'S.StateT' in the base monad
runStateP
    :: Monad m
    => s
    -> Proxy a' a b' b (S.StateT s m) r
    -> Proxy a' a b' b m (r, s)
runStateP s p = (`S.runStateT` s) $ distribute p
{-# INLINABLE runStateP #-}

-- | Evaluate 'S.StateT' in the base monad
evalStateP
    :: Monad m
    => s
    -> Proxy a' a b' b (S.StateT s m) r
    -> Proxy a' a b' b m r
evalStateP s p = fmap fst $ runStateP s p
{-# INLINABLE evalStateP #-}

-- | Execute 'S.StateT' in the base monad
execStateP
    :: Monad m
    => s
    -> Proxy a' a b' b (S.StateT s m) r
    -> Proxy a' a b' b m s
execStateP s p = fmap snd $ runStateP s p
{-# INLINABLE execStateP #-}

{- $writert
    Note that 'runWriterP' and 'execWriterP' will keep the accumulator in
    weak-head-normal form so that folds run in constant space when possible.

    This means that until @transformers@ adds a truly strict 'W.WriterT', you
    should consider unwrapping 'W.WriterT' first using 'runWriterP' or
    'execWriterP' before running your 'Proxy'.  You will get better performance
    this way and eliminate space leaks if your accumulator doesn't have any lazy
    fields.
-}

-- | Wrap the base monad in 'W.WriterT'
writerP
    :: (Monad m, Monoid w)
    => Proxy a' a b' b m (r, w) -> Proxy a' a b' b (W.WriterT w m) r
writerP p = do
    (r, w) <- unsafeHoist lift p
    lift $ W.tell w
    return r
{-# INLINABLE writerP #-}

-- | Run 'W.WriterT' in the base monad
runWriterP
    :: (Monad m, Monoid w)
    => Proxy a' a b' b (W.WriterT w m) r
    -> Proxy a' a b' b m (r, w)
runWriterP p = W.runWriterT $ distribute p
{-# INLINABLE runWriterP #-}

-- | Execute 'W.WriterT' in the base monad
execWriterP
    :: (Monad m, Monoid w)
    => Proxy a' a b' b (W.WriterT w m) r
    -> Proxy a' a b' b m w
execWriterP p = fmap snd $ runWriterP p
{-# INLINABLE execWriterP #-}

-- | Wrap the base monad in 'RWS.RWST'
rwsP
    :: (Monad m, Monoid w)
    => (i -> s -> Proxy a' a b' b m (r, s, w))
    -> Proxy a' a b' b (RWS.RWST i w s m) r
rwsP k = do
    i <- lift RWS.ask
    s <- lift RWS.get
    (r, s', w) <- unsafeHoist lift (k i s)
    lift $ do
        RWS.put s'
        RWS.tell w
    return r
{-# INLINABLE rwsP #-}

-- | Run 'RWS.RWST' in the base monad
runRWSP
    :: (Monad m, Monoid w)
    => r
    -> s
    -> Proxy a' a b' b (RWS.RWST r w s m) d
    -> Proxy a' a b' b m (d, s, w)
runRWSP  i s p = (\b -> RWS.runRWST b i s) $ distribute p
{-# INLINABLE runRWSP #-}

-- | Evaluate 'RWS.RWST' in the base monad
evalRWSP
    :: (Monad m, Monoid w)
    => r
    -> s
    -> Proxy a' a b' b (RWS.RWST r w s m) d
    -> Proxy a' a b' b m (d, w)
evalRWSP i s p = fmap f $ runRWSP i s p
  where
    f x = let (r, _, w) = x in (r, w)
{-# INLINABLE evalRWSP #-}

-- | Execute 'RWS.RWST' in the base monad
execRWSP
    :: (Monad m, Monoid w)
    => r
    -> s
    -> Proxy a' a b' b (RWS.RWST r w s m) d
    -> Proxy a' a b' b m (s, w)
execRWSP i s p = fmap f $ runRWSP i s p
  where
    f x = let (_, s', w) = x in (s', w)
{-# INLINABLE execRWSP #-}

{- $tutorial
    Probably the most useful functionality in this module is lifted error
    handling.  Suppose that you have a 'Pipes.Pipe' whose base monad can fail
    using 'E.ErrorT':

> import Control.Monad.Trans.Error
> import Pipes
>
> example :: Monad m => Pipe Int Int (ErrorT String m) r
> example = for cat $ \n ->
>     if n == 0
>     then lift $ throwError "Zero is forbidden"
>     else yield n

    Without the tools in this module you cannot recover from any potential error
    until after you compose and run the pipeline:

>>> import qualified Pipes.Prelude as P
>>> runErrorT $ runEffect $ P.readLn >-> example >-> P.print
42<Enter>
42
1<Enter>
1
0<Enter>
Zero is forbidden
>>>

    This module provides `catchError`, which lets you catch and recover from
    errors inside the 'Pipe':

>  import qualified Pipes.Lift as Lift
> 
>  caught :: Pipe Int Int (ErrorT String IO) r
>  caught = example `Lift.catchError` \str -> do
>      liftIO (putStrLn str)
>      caught

    This lets you resume streaming in the face of errors raised within the base
    monad:

>>> runErrorT $ runEffect $ P.readLn >-> caught >-> P.print
0<Enter>
Zero is forbidden
42<Enter>
42
0<Enter>
Zero is forbidden
1<Enter>
1
...

    Another common use case is running a base monad before running the pipeline.
    For example, the following contrived 'Producer' uses 'S.StateT' gratuitously
    to increment numbers:

> import Control.Monad (forever)
> import Control.Monad.Trans.State.Strict
> import Pipes
> 
> numbers :: Monad m => Producer Int (StateT Int m) r
> numbers = forever $ do
>     n <- lift get
>     yield n
>     lift $ put $! n + 1

    You can run the 'StateT' monad by supplying an initial state, before you
    ever compose the 'Producer':

> import Pipes.Lift
>
> naturals :: Monad m => Producer Int m r
> naturals = evalStateP 0 numbers

    This deletes 'StateT' from the base monad entirely, give you a completely
    pure 'Pipes.Producer':

>>> Pipes.Prelude.toList naturals
[0,1,2,3,4,5,6...]

    Note that the convention for the 'S.StateT' run functions is backwards from
    @transformers@ for convenience: the initial state is the first argument.

    All of these functions internally use 'distribute', which can pull out most
    monad transformers from the base monad.  For example, 'evalStateP' is
    defined in terms of 'distribute':

> evalStateP s p = evalStateT (distribute p) s

    Therefore you can use 'distribute' to run other monad transformers, too, as
    long as they implement the 'MFunctor' type class from the @mmorph@ library.
-}
