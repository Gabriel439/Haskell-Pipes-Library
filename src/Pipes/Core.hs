{-| The core functionality for the 'Proxy' monad transformer

    Read "Pipes.Tutorial" if you want a beginners tutorial explaining how to use
    this library.  The documentation in this module targets more advanced users
    who want to understand the theory behind this library.

    This module is not exported by default, and I recommend you use the
    unidirectional operations exported by the "Pipes" module if you can.  You
    should only use this module if you require advanced features like:

    * bidirectional communication, or:

    * push-based 'Pipe's.
-}

{-# LANGUAGE RankNTypes, Trustworthy #-}

module Pipes.Core (
    -- * Proxy Monad Transformer
    -- $proxy
      Proxy
    , runEffect

    -- * Categories
    -- $categories

    -- ** Respond
    -- $respond
    , respond
    , (/>/)
    , (//>)

    -- ** Request
    -- $request
    , request
    , (\>\)
    , (>\\)

    -- ** Push
    -- $push
    , push
    , (>~>)
    , (>>~)

    -- ** Pull
    -- $pull
    , pull
    , (>+>)
    , (+>>)

    -- ** Reflect
    -- $reflect
    , reflect

    -- * Concrete Type Synonyms
    , X
    , Effect
    , Producer
    , Pipe
    , Consumer
    , Client
    , Server

    -- * Polymorphic Type Synonyms
    , Effect'
    , Producer'
    , Consumer'
    , Client'
    , Server'

    -- * Flipped operators
    , (\<\)
    , (/</)
    , (<~<)
    , (~<<)
    , (<+<)
    , (<\\)
    , (//<)
    , (<<+)

    -- * Re-exports
    , closed
    ) where

import Pipes.Internal (Proxy(..), X, closed, fixl, fixr)

{- $proxy
    Diagrammatically, you can think of a 'Proxy' as having the following shape:

@
 Upstream | Downstream
     +---------+
     |         |
 a' <==       <== b'
     |         |
 a  ==>       ==> b
     |    |    |
     +----|----+
          v
          r
@

    You can connect proxies together in five different ways:

    * ('Pipes.>+>'): connect pull-based streams

    * ('Pipes.>~>'): connect push-based streams

    * ('Pipes.\>\'): chain folds

    * ('Pipes./>/'): chain unfolds

    * ('Control.Monad.>=>'): sequence proxies

-}

-- | Run a self-contained 'Effect', converting it back to the base monad
runEffect :: Monad m => Effect m r -> m r
runEffect = go
  where
    go p = case p of
        Request v _ -> closed v
        Respond v _ -> closed v
        M       m   -> m >>= go
        Pure    r   -> return r
{-# INLINABLE runEffect #-}

{- * Keep proxy composition lower in precedence than function composition, which
     is 9 at the time of of this comment, so that users can write things like:


> lift . k >+> p
>
> hoist f . k >+> p

   * Keep the priorities different so that users can mix composition operators
     like:

> up \>\ p />/ dn
>
> up >~> p >+> dn

   * Keep 'request' and 'respond' composition lower in precedence than 'pull'
     and 'push' composition, so that users can do:

> read \>\ pull >+> writer

   * I arbitrarily choose a lower priority for downstream operators so that lazy
     pull-based computations need not evaluate upstream stages unless absolutely
     necessary.
-}
infixl 3 //>
infixr 3 <\\      -- GHC will raise a parse error if either of these lines ends
infixr 4 />/, >\\ -- with '\', which is why this comment is here
infixl 4 \<\, //<
infixl 5 \>\      -- Same thing here
infixr 5 /</
infixl 6 <<+
infixr 6 +>>
infixl 7 >+>, >>~
infixr 7 <+<, ~<<
infixl 8 <~<
infixr 8 >~>

{- $categories
    A 'Control.Category.Category' is a set of components that you can connect
    with a composition operator, ('Control.Category..'), that has an identity,
    'Control.Category.id'.  The ('Control.Category..') and 'Control.Category.id'
    must satisfy the following three 'Control.Category.Category' laws:

@
\-\- Left identity
'Control.Category.id' 'Control.Category..' f = f

\-\- Right identity
f 'Control.Category..' 'Control.Category.id' = f

\-\- Associativity
(f 'Control.Category..' g) 'Control.Category..' h = f 'Control.Category..' (g 'Control.Category..' h)
@

    The 'Proxy' type sits at the intersection of five separate categories, four
    of which are named after their identity:

@
                     Identity   | Composition |  Point-ful
                  +-------------+-------------+-------------+
 respond category |   'respond'   |     '/>/'     |     '//>'     |
 request category |   'request'   |     '\>\'     |     '>\\'     |
    push category |   'push'      |     '>~>'     |     '>>~'     |
    pull category |   'pull'      |     '>+>'     |     '+>>'     |
 Kleisli category |   'return'    |     'Control.Monad.>=>'     |     '>>='     |
                  +-------------+-------------+-------------+
@

    Each composition operator has a \"point-ful\" version, analogous to how
    ('>>=') is the point-ful version of ('Control.Monad.>=>').  For example,
    ('//>') is the point-ful version of ('/>/').  The convention is that the
    odd character out faces the argument that is a function.
-}

{- $respond
    The 'respond' category closely corresponds to the generator design pattern.

    The 'respond' category obeys the category laws, where 'respond' is the
    identity and ('/>/') is composition:

@
\-\- Left identity
'respond' '/>/' f = f

\-\- Right identity
f '/>/' 'respond' = f

\-\- Associativity
(f '/>/' g) '/>/' h = f '/>/' (g '/>/' h)
@

#respond-diagram#

    The following diagrams show the flow of information:

@
'respond' :: 'Monad' m
       =>  a -> 'Proxy' x' x a' a m a'

\          a
          |
     +----|----+
     |    |    |
 x' <==   \\ /==== a'
     |     X   |
 x  ==>   / \\===> a
     |    |    |
     +----|----+
          v
          a'

('/>/') :: 'Monad' m
      => (a -> 'Proxy' x' x b' b m a')
      -> (b -> 'Proxy' x' x c' c m b')
      -> (a -> 'Proxy' x' x c' c m a')

\          a                   /===> b                      a
          |                  /      |                      |
     +----|----+            /  +----|----+            +----|----+
     |    v    |           /   |    v    |            |    v    |
 x' <==       <== b' <==\\ / x'<==       <== c'    x' <==       <== c'
     |    f    |         X     |    g    |     =      | f '/>/' g |
 x  ==>       ==> b  ===/ \\ x ==>       ==> c     x  ==>       ==> c'
     |    |    |           \\   |    |    |            |    |    |
     +----|----+            \\  +----|----+            +----|----+
          v                  \\      v                      v
          a'                  \\==== b'                     a'

('//>') :: 'Monad' m
      => 'Proxy' x' x b' b m a'
      -> (b -> 'Proxy' x' x c' c m b')
      -> 'Proxy' x' x c' c m a'

\                              /===> b
                             /      |
     +---------+            /  +----|----+            +---------+
     |         |           /   |    v    |            |         |
 x' <==       <== b' <==\\ / x'<==       <== c'    x' <==       <== c'
     |    f    |         X     |    g    |     =      | f '//>' g |
 x  ==>       ==> b  ===/ \\ x ==>       ==> c     x  ==>       ==> c'
     |    |    |           \\   |    |    |            |    |    |
     +----|----+            \\  +----|----+            +----|----+
          v                  \\      v                      v
          a'                  \\==== b'                     a'
@

-}

{-| Send a value of type @a@ downstream and block waiting for a reply of type
    @a'@

    'respond' is the identity of the respond category.
-}
respond :: Monad m => a -> Proxy x' x a' a m a'
respond a = Respond a Pure
{-# INLINABLE respond #-}

{-| Compose two unfolds, creating a new unfold

@
(f '/>/' g) x = f x '//>' g
@

    ('/>/') is the composition operator of the respond category.
-}
(/>/)
    :: Monad m
    => (a -> Proxy x' x b' b m a')
    -- ^
    -> (b -> Proxy x' x c' c m b')
    -- ^
    -> (a -> Proxy x' x c' c m a')
    -- ^
(fa />/ fb) a = fa a //> fb
{-# INLINABLE (/>/) #-}

{-| @(p \/\/> f)@ replaces each 'respond' in @p@ with @f@.

    Point-ful version of ('/>/')
-}
(//>)
    :: Monad m
    =>       Proxy x' x b' b m a'
    -- ^
    -> (b -> Proxy x' x c' c m b')
    -- ^
    ->       Proxy x' x c' c m a'
    -- ^
p0 //> fb = go p0
  where
    go p = case p of
        Respond b  fb' -> fb b >>= \b' -> go (fb' b')
        _              -> fixr go p
{-# INLINABLE (//>) #-}

{-# RULES
    "(Request x' fx ) //> fb" forall x' fx  fb .
        (Request x' fx ) //> fb = Request x' (\x -> fx x //> fb);
    "(Respond b  fb') //> fb" forall b  fb' fb .
        (Respond b  fb') //> fb = fb b >>= \b' -> fb' b' //> fb;
    "(M          m  ) //> fb" forall    m   fb .
        (M          m  ) //> fb = M (m >>= \p' -> return (p' //> fb));
    "(Pure      a   ) //> fb" forall a      fb .
        (Pure    a     ) //> fb = Pure a;
  #-}

{- $request
    The 'request' category closely corresponds to the iteratee design pattern.

    The 'request' category obeys the category laws, where 'request' is the
    identity and ('\>\') is composition:

@
-- Left identity
'request' '\>\' f = f

\-\- Right identity
f '\>\' 'request' = f

\-\- Associativity
(f '\>\' g) '\>\' h = f '\>\' (g '\>\' h)
@

#request-diagram#

    The following diagrams show the flow of information:

@
'request' :: 'Monad' m
        =>  a' -> 'Proxy' a' a y' y m a

\          a'
          |
     +----|----+
     |    |    |
 a' <=====/   <== y'
     |         |
 a  ======\\   ==> y
     |    |    |
     +----|----+
          v
          a

('\>\') :: 'Monad' m
      => (b' -> 'Proxy' a' a y' y m b)
      -> (c' -> 'Proxy' b' b y' y m c)
      -> (c' -> 'Proxy' a' a y' y m c)

\          b'<=====\\                c'                     c'
          |        \\               |                      |
     +----|----+    \\         +----|----+            +----|----+
     |    v    |     \\        |    v    |            |    v    |
 a' <==       <== y'  \\== b' <==       <== y'    a' <==       <== y'
     |    f    |              |    g    |     =      | f '\>\' g |
 a  ==>       ==> y   /=> b  ==>       ==> y     a  ==>       ==> y
     |    |    |     /        |    |    |            |    |    |
     +----|----+    /         +----|----+            +----|----+
          v        /               v                      v
          b ======/                c                      c

('>\\') :: Monad m
      => (b' -> Proxy a' a y' y m b)
      -> Proxy b' b y' y m c
      -> Proxy a' a y' y m c

\          b'<=====\\
          |        \\
     +----|----+    \\         +---------+            +---------+
     |    v    |     \\        |         |            |         |
 a' <==       <== y'  \\== b' <==       <== y'    a' <==       <== y'
     |    f    |              |    g    |     =      | f '>\\' g |
 a  ==>       ==> y   /=> b  ==>       ==> y     a  ==>       ==> y
     |    |    |     /        |    |    |            |    |    |
     +----|----+    /         +----|----+            +----|----+
          v        /               v                      v
          b ======/                c                      c
@
-}

{-| Send a value of type @a'@ upstream and block waiting for a reply of type @a@

    'request' is the identity of the request category.
-}
request :: Monad m => a' -> Proxy a' a y' y m a
request a' = Request a' Pure
{-# INLINABLE request #-}

{-| Compose two folds, creating a new fold

@
(f '\>\' g) x = f '>\\' g x
@

    ('\>\') is the composition operator of the request category.
-}
(\>\)
    :: Monad m
    => (b' -> Proxy a' a y' y m b)
    -- ^
    -> (c' -> Proxy b' b y' y m c)
    -- ^
    -> (c' -> Proxy a' a y' y m c)
    -- ^
(fb' \>\ fc') c' = fb' >\\ fc' c'
{-# INLINABLE (\>\) #-}

{-| @(f >\\\\ p)@ replaces each 'request' in @p@ with @f@.

    Point-ful version of ('\>\')
-}
(>\\)
    :: Monad m
    => (b' -> Proxy a' a y' y m b)
    -- ^
    ->        Proxy b' b y' y m c
    -- ^
    ->        Proxy a' a y' y m c
    -- ^
fb' >\\ p0 = go p0
  where
    go p = case p of
        Request b' fb  -> fb' b' >>= \b -> go (fb b)
        _              -> fixl go p
{-# INLINABLE (>\\) #-}

{-# RULES
    "fb' >\\ (Request b' fb )" forall fb' b' fb  .
        fb' >\\ (Request b' fb ) = fb' b' >>= \b -> fb' >\\ fb  b;
    "fb' >\\ (Respond x  fx')" forall fb' x  fx' .
        fb' >\\ (Respond x  fx') = Respond x (\x' -> fb' >\\ fx' x');
    "fb' >\\ (M          m  )" forall fb'    m   .
        fb' >\\ (M          m  ) = M (m >>= \p' -> return (fb' >\\ p'));
    "fb' >\\ (Pure    a    )" forall fb' a      .
        fb' >\\ (Pure    a     ) = Pure a;
  #-}

{- $push
    The 'push' category closely corresponds to push-based Unix pipes.

    The 'push' category obeys the category laws, where 'push' is the identity
    and ('>~>') is composition:

@
\-\- Left identity
'push' '>~>' f = f

\-\- Right identity
f '>~>' 'push' = f

\-\- Associativity
(f '>~>' g) '>~>' h = f '>~>' (g '>~>' h)
@

    The following diagram shows the flow of information:

@
'push'  :: 'Monad' m
      =>  a -> 'Proxy' a' a a' a m r

\          a
          |
     +----|----+
     |    v    |
 a' <============ a'
     |         |
 a  ============> a
     |    |    |
     +----|----+
          v
          r

('>~>') :: 'Monad' m
      => (a -> 'Proxy' a' a b' b m r)
      -> (b -> 'Proxy' b' b c' c m r)
      -> (a -> 'Proxy' a' a c' c m r)

\          a                b                      a
          |                |                      |
     +----|----+      +----|----+            +----|----+
     |    v    |      |    v    |            |    v    |
 a' <==       <== b' <==       <== c'    a' <==       <== c'
     |    f    |      |    g    |     =      | f '>~>' g |
 a  ==>       ==> b  ==>       ==> c     a  ==>       ==> c
     |    |    |      |    |    |            |    |    |
     +----|----+      +----|----+            +----|----+
          v                v                      v
          r                r                      r
@

-}

{-| Forward responses followed by requests

@
'push' = 'respond' 'Control.Monad.>=>' 'request' 'Control.Monad.>=>' 'push'
@

    'push' is the identity of the push category.
-}
push :: Monad m => a -> Proxy a' a a' a m r
push = go
  where
    go a = Respond a (\a' -> Request a' go)
{-# INLINABLE push #-}

{-| Compose two proxies blocked while 'request'ing data, creating a new proxy
    blocked while 'request'ing data

@
(f '>~>' g) x = f x '>>~' g
@

    ('>~>') is the composition operator of the push category.
-}
(>~>)
    :: Monad m
    => (_a -> Proxy a' a b' b m r)
    -- ^
    -> ( b -> Proxy b' b c' c m r)
    -- ^
    -> (_a -> Proxy a' a c' c m r)
    -- ^
(fa >~> fb) a = fa a >>~ fb
{-# INLINABLE (>~>) #-}

{-| @(p >>~ f)@ pairs each 'respond' in @p@ with a 'request' in @f@.

    Point-ful version of ('>~>')
-}
(>>~)
    :: Monad m
    =>       Proxy a' a b' b m r
    -- ^
    -> (b -> Proxy b' b c' c m r)
    -- ^
    ->       Proxy a' a c' c m r
    -- ^
p >>~ fb = case p of
    Respond b  fb' -> fb' +>> fb b
    _              -> fixr (>>~ fb) p
{-# INLINABLE (>>~) #-}

{- $pull
    The 'pull' category closely corresponds to pull-based Unix pipes.

    The 'pull' category obeys the category laws, where 'pull' is the identity
    and ('>+>') is composition:

@
\-\- Left identity
'pull' '>+>' f = f

\-\- Right identity
f '>+>' 'pull' = f

\-\- Associativity
(f '>+>' g) '>+>' h = f '>+>' (g '>+>' h)
@

#pull-diagram#

    The following diagrams show the flow of information:

@
'pull'  :: 'Monad' m
      =>  a' -> 'Proxy' a' a a' a m r

\          a'
          |
     +----|----+
     |    v    |
 a' <============ a'
     |         |
 a  ============> a
     |    |    |
     +----|----+
          v
          r

('>+>') :: 'Monad' m
      -> (b' -> 'Proxy' a' a b' b m r)
      -> (c' -> 'Proxy' b' b c' c m r)
      -> (c' -> 'Proxy' a' a c' c m r)

\          b'               c'                     c'
          |                |                      |
     +----|----+      +----|----+            +----|----+
     |    v    |      |    v    |            |    v    |
 a' <==       <== b' <==       <== c'    a' <==       <== c'
     |    f    |      |    g    |     =      | f >+> g |
 a  ==>       ==> b  ==>       ==> c     a  ==>       ==> c
     |    |    |      |    |    |            |    |    |
     +----|----+      +----|----+            +----|----+
          v                v                      v
          r                r                      r
@

-}

{-| Forward requests followed by responses:

@
'pull' = 'request' 'Control.Monad.>=>' 'respond' 'Control.Monad.>=>' 'pull'
@

    'pull' is the identity of the pull category.
-}
pull :: Monad m => a' -> Proxy a' a a' a m r
pull = go
  where
    go a' = Request a' (\a -> Respond a go)
{-# INLINABLE pull #-}

{-| Compose two proxies blocked in the middle of 'respond'ing, creating a new
    proxy blocked in the middle of 'respond'ing

@
(f '>+>' g) x = f '+>>' g x
@

    ('>+>') is the composition operator of the pull category.
-}
(>+>)
    :: Monad m
    => ( b' -> Proxy a' a b' b m r)
    -- ^
    -> (_c' -> Proxy b' b c' c m r)
    -- ^
    -> (_c' -> Proxy a' a c' c m r)
    -- ^
(fb' >+> fc') c' = fb' +>> fc' c'
{-# INLINABLE (>+>) #-}

{-| @(f +>> p)@ pairs each 'request' in @p@ with a 'respond' in @f@.

    Point-ful version of ('>+>')
-}
(+>>)
    :: Monad m
    => (b' -> Proxy a' a b' b m r)
    -- ^
    ->        Proxy b' b c' c m r
    -- ^
    ->        Proxy a' a c' c m r
    -- ^
fb' +>> p = case p of
    Request b' fb  -> fb' b' >>~ fb
    _              -> fixl (fb' +>>) p
{-# INLINABLE (+>>) #-}

{- $reflect
    @(reflect .)@ transforms each streaming category into its dual:

    * The request category is the dual of the respond category

@
'reflect' '.' 'respond' = 'request'

'reflect' '.' (f '/>/' g) = 'reflect' '.' f '/</' 'reflect' '.' g
@

@
'reflect' '.' 'request' = 'respond'

'reflect' '.' (f '\>\' g) = 'reflect' '.' f '\<\' 'reflect' '.' g
@

    * The pull category is the dual of the push category

@
'reflect' '.' 'push' = 'pull'

'reflect' '.' (f '>~>' g) = 'reflect' '.' f '<+<' 'reflect' '.' g
@

@
'reflect' '.' 'pull' = 'push'

'reflect' '.' (f '>+>' g) = 'reflect' '.' f '<~<' 'reflect' '.' g
@
-}

-- | Switch the upstream and downstream ends
reflect :: Monad m => Proxy a' a b' b m r -> Proxy b b' a a' m r
reflect = go
  where
    go p = case p of
        Request a' fa  -> Respond a' (\a  -> go (fa  a ))
        Respond b  fb' -> Request b  (\b' -> go (fb' b'))
        M          m   -> M (m >>= \p' -> return (go p'))
        Pure    r      -> Pure r
{-# INLINABLE reflect #-}

{-| An effect in the base monad

    'Effect's neither 'Pipes.await' nor 'Pipes.yield'
-}
type Effect = Proxy X () () X

-- | 'Producer's can only 'Pipes.yield'
type Producer b = Proxy X () () b

-- | 'Pipe's can both 'Pipes.await' and 'Pipes.yield'
type Pipe a b = Proxy () a () b

-- | 'Consumer's can only 'Pipes.await'
type Consumer a = Proxy () a () X

{-| @Client a' a@ sends requests of type @a'@ and receives responses of
    type @a@.

    'Client's only 'request' and never 'respond'.
-}
type Client a' a = Proxy a' a () X

{-| @Server b' b@ receives requests of type @b'@ and sends responses of type
    @b@.

    'Server's only 'respond' and never 'request'.
-}
type Server b' b = Proxy X () b' b

-- | Like 'Effect', but with a polymorphic type
type Effect' m r = forall x' x y' y . Proxy x' x y' y m r

-- | Like 'Producer', but with a polymorphic type
type Producer' b m r = forall x' x . Proxy x' x () b m r

-- | Like 'Consumer', but with a polymorphic type
type Consumer' a m r = forall y' y . Proxy () a y' y m r

-- | Like 'Server', but with a polymorphic type
type Server' b' b m r = forall x' x . Proxy x' x b' b m r

-- | Like 'Client', but with a polymorphic type
type Client' a' a m r = forall y' y . Proxy a' a y' y m r

-- | Equivalent to ('/>/') with the arguments flipped
(\<\)
    :: Monad m
    => (b -> Proxy x' x c' c m b')
    -- ^
    -> (a -> Proxy x' x b' b m a')
    -- ^
    -> (a -> Proxy x' x c' c m a')
    -- ^
p1 \<\ p2 = p2 />/ p1
{-# INLINABLE (\<\) #-}

-- | Equivalent to ('\>\') with the arguments flipped
(/</)
    :: Monad m
    => (c' -> Proxy b' b x' x m c)
    -- ^
    -> (b' -> Proxy a' a x' x m b)
    -- ^
    -> (c' -> Proxy a' a x' x m c)
    -- ^
p1 /</ p2 = p2 \>\ p1
{-# INLINABLE (/</) #-}

-- | Equivalent to ('>~>') with the arguments flipped
(<~<)
    :: Monad m
    => (b -> Proxy b' b c' c m r)
    -- ^
    -> (a -> Proxy a' a b' b m r)
    -- ^
    -> (a -> Proxy a' a c' c m r)
    -- ^
p1 <~< p2 = p2 >~> p1
{-# INLINABLE (<~<) #-}

-- | Equivalent to ('>+>') with the arguments flipped
(<+<)
    :: Monad m
    => (c' -> Proxy b' b c' c m r)
    -- ^
    -> (b' -> Proxy a' a b' b m r)
    -- ^
    -> (c' -> Proxy a' a c' c m r)
    -- ^
p1 <+< p2 = p2 >+> p1
{-# INLINABLE (<+<) #-}

-- | Equivalent to ('//>') with the arguments flipped
(<\\)
    :: Monad m
    => (b -> Proxy x' x c' c m b')
    -- ^
    ->       Proxy x' x b' b m a'
    -- ^
    ->       Proxy x' x c' c m a'
    -- ^
f <\\ p = p //> f
{-# INLINABLE (<\\) #-}

-- | Equivalent to ('>\\') with the arguments flipped
(//<)
    :: Monad m
    =>        Proxy b' b y' y m c
    -- ^
    -> (b' -> Proxy a' a y' y m b)
    -- ^
    ->        Proxy a' a y' y m c
    -- ^
p //< f = f >\\ p
{-# INLINABLE (//<) #-}

-- | Equivalent to ('>>~') with the arguments flipped
(~<<)
    :: Monad m
    => (b  -> Proxy b' b c' c m r)
    -- ^
    ->        Proxy a' a b' b m r
    -- ^
    ->        Proxy a' a c' c m r
    -- ^
k ~<< p = p >>~ k
{-# INLINABLE (~<<) #-}

-- | Equivalent to ('+>>') with the arguments flipped
(<<+)
    :: Monad m
    =>         Proxy b' b c' c m r
    -- ^
    -> (b'  -> Proxy a' a b' b m r)
    -- ^
    ->         Proxy a' a c' c m r
    -- ^
k <<+ p = p +>> k
{-# INLINABLE (<<+) #-}

{-# RULES
    "(p //> f) //> g" forall p f g . (p //> f) //> g = p //> (\x -> f x //> g)

  ; "p //> respond" forall p . p //> respond = p

  ; "respond x //> f" forall x f . respond x //>  f = f x

  ; "f >\\ (g >\\ p)" forall f g p . f >\\ (g >\\ p) = (\x -> f >\\ g x) >\\ p

  ; "request >\\ p" forall p . request >\\ p = p

  ; "f >\\ request x" forall f x . f >\\ request x = f x

  ; "(p >>~ f) >>~ g" forall p f g . (p >>~ f) >>~ g = p >>~ (\x -> f x >>~ g)

  ; "p >>~ push" forall p . p >>~ push = p

  ; "push x >>~ f" forall x f . push x >>~ f = f x

  ; "f +>> (g +>> p)" forall f g p . f +>> (g +>> p) = (\x -> f +>> g x) +>> p

  ; "pull +>> p" forall p . pull +>> p = p

  ; "f +>> pull x" forall f x . f +>> pull x = f x

  #-}
