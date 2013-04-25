-- | General purpose proxies

module Control.Proxy.Prelude.Base (
    -- * Maps
    mapD,
    mapU,
    mapB,
    mapMD,
    mapMU,
    mapMB,
    useD,
    useU,
    useB,
    execD,
    execU,
    execB,

    -- * Filters
    takeB,
    takeB_,
    takeWhileD,
    takeWhileU,
    dropD,
    dropU,
    dropWhileD,
    dropWhileU,
    filterD,
    filterU,

    -- * Lists
    fromListS,
    fromListC,

    -- * Enumerations
    enumFromS,
    enumFromC,
    enumFromToS,
    enumFromToC,

    -- * ListT
    eachS,
    eachC,
    rangeS,
    rangeC,

    -- * Folds
    foldD,
    foldU,
    allD,
    allU,
    allD_,
    allU_,
    anyD,
    anyU,
    anyD_,
    anyU_,
    sumD,
    sumU,
    productD,
    productU,
    lengthD,
    lengthU,
    headD,
    headD_,
    headU,
    headU_,
    lastD,
    lastU,
    toListD,
    toListU,
    foldrD,
    foldrU,

    -- * ArrowChoice
    -- $choice
    leftD,
    rightD,
    leftU,
    rightU,

    -- * Zips and Merges
    zipD,
    mergeD,

    -- * Closed Adapters
    -- $open
    unitD,
    unitU,

    -- * Modules
    -- $modules
    module Control.Proxy.Trans.Writer,
    module Data.Monoid
    ) where

import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Class (lift)

import Control.Proxy.Trans.Writer (
    WriterP, runWriterP, runWriterK, execWriterP, execWriterK )
import qualified Control.Proxy.Trans.Writer as W
import Control.Proxy.Class
import Control.Proxy.ListT (
    RespondT(RespondT),
    RequestT(RequestT),
    ProduceT,
    CoProduceT )
import Control.Proxy.Synonym (Producer, Consumer, Pipe, CoProducer, CoPipe)
import Control.Proxy.Trans.Identity (runIdentityP, runIdentityK, identityK)
import Data.Monoid (
    Monoid,
    Endo(Endo, appEndo),
    All(All, getAll),
    Any(Any, getAny),
    Sum(Sum, getSum),
    Product(Product, getProduct),
    First(First, getFirst),
    Last(Last, getLast) )

{-| @(mapD f)@ applies @f@ to all values going \'@D@\'ownstream.

> mapD f1 >-> mapD f2 = mapD (f2 . f1)
>
> mapD id = idPull
-}
mapD :: (Monad m, Proxy p) => (a -> b) -> x -> p x a x b m r
mapD f = runIdentityK go where
    go x = do
        a  <- request x
        x2 <- respond (f a)
        go x2
-- mapD f = runIdentityK $ foreverK $ request >=> respond . f
{-# INLINABLE mapD #-}

{-| @(mapU g)@ applies @g@ to all values going \'@U@\'pstream.

> mapU g1 >-> mapU g2 = mapU (g1 . g2)
>
> mapU id = idPull
-}
mapU :: (Monad m, Proxy p) => (b' -> a') -> b' -> p a' x b' x m r
mapU g = runIdentityK go where
    go b' = do
        x   <- request (g b')
        b'2 <- respond x
        go b'2
-- mapU g = foreverK $ (request . g) >=> respond
{-# INLINABLE mapU #-}

{-| @(mapB f g)@ applies @f@ to all values going downstream and @g@ to all
    values going upstream.

    Mnemonic: map \'@B@\'idirectional

> mapB f1 g1 >-> mapB f2 g2 = mapB (f2 . f1) (g1 . g2)
>
> mapB id id = idPull
-}
mapB :: (Monad m, Proxy p) => (a -> b) -> (b' -> a') -> b' -> p a' a b' b m r
mapB f g = runIdentityK go where
    go b' = do
        a   <- request (g b')
        b'2 <- respond (f a )
        go b'2
-- mapB f g = foreverK $ request . g >=> respond . f
{-# INLINABLE mapB #-}

{-| @(mapMD f)@ applies the monadic function @f@ to all values going downstream

> mapMD f1 >-> mapMD f2 = mapMD (f1 >=> f2)
>
> mapMD return = idPull
-}
mapMD :: (Monad m, Proxy p) => (a -> m b) -> x -> p x a x b m r
mapMD f = runIdentityK go where
    go x = do
        a  <- request x
        b  <- lift (f a)
        x2 <- respond b
        go x2
-- mapMD f = foreverK $ request >=> lift . f >=> respond
{-# INLINABLE mapMD #-}

{-| @(mapMU g)@ applies the monadic function @g@ to all values going upstream

> mapMU g1 >-> mapMU g2 = mapMU (g2 >=> g1)
>
> mapMU return = idPull
-}
mapMU :: (Monad m, Proxy p) => (b' -> m a') -> b' -> p a' x b' x m r
mapMU g = runIdentityK go where
    go b' = do
        a'  <- lift (g b')
        x   <- request a'
        b'2 <- respond x
        go b'2
-- mapMU g = foreverK $ lift . g >=> request >=> respond
{-# INLINABLE mapMU #-}

{-| @(mapMB f g)@ applies the monadic function @f@ to all values going
    downstream and the monadic function @g@ to all values going upstream.

> mapMB f1 g1 >-> mapMB f2 g2 = mapMB (f1 >=> f2) (g2 >=> g1)
>
> mapMB return return = idPull
-}
mapMB
    :: (Monad m, Proxy p) => (a -> m b) -> (b' -> m a') -> b' -> p a' a b' b m r
mapMB f g = runIdentityK go where
    go b' = do
        a'  <- lift (g b')
        a   <- request a'
        b   <- lift (f a )
        b'2 <- respond b
        go b'2
-- mapMB f g = foreverK $ lift . g >=> request >=> lift . f >=> respond
{-# INLINABLE mapMB #-}

{-| @(useD f)@ executes the monadic function @f@ on all values flowing
    \'@D@\'ownstream

> useD f1 >-> useD f2 = useD (\a -> f1 a >> f2 a)
>
> useD (\_ -> return ()) = idPull
-}
useD :: (Monad m, Proxy p) => (a -> m r1) -> x -> p x a x a m r
useD f = runIdentityK go where
    go x = do
        a  <- request x
        _  <- lift $ f a
        x2 <- respond a
        go x2
{-# INLINABLE useD #-}

{-| @(useU g)@ executes the monadic function @g@ on all values flowing
    \'@U@\'pstream

> useU g1 >-> useU g2 = useU (\a' -> g2 a' >> g1 a')
>
> useU (\_ -> return ()) = idPull
-}
useU :: (Monad m, Proxy p) => (a' -> m r2) -> a' -> p a' x a' x m r
useU g = runIdentityK go where
    go a' = do
        _   <- lift $ g a'
        x   <- request a'
        a'2 <- respond x
        go a'2
{-# INLINABLE useU #-}

{-| @(useB f g)@ executes the monadic function @f@ on all values flowing
    downstream and the monadic function @g@ on all values flowing upstream

> useB f1 g1 >-> useB f2 g2 = useB (\a -> f1 a >> f2 a) (\a' -> g2 a' >> g1 a')
>
> useB (\_ -> return ()) (\_ -> return ()) = idPull
-}
useB
    :: (Monad m, Proxy p)
    => (a -> m r1) -> (a' -> m r2) -> a' -> p a' a a' a m r
useB f g = runIdentityK go where
    go a' = do
        _   <- lift $ g a'
        a   <- request a'
        _   <- lift $ f a
        a'2 <- respond a
        go a'2
{-# INLINABLE useB #-}

{-| @(execD md)@ executes @md@ every time values flow downstream through it.

> execD md1 >-> execD md2 = execD (md1 >> md2)
>
> execD (return ()) = idPull
-}
execD :: (Monad m, Proxy p) => m r1 -> a' -> p a' a a' a m r
execD md = runIdentityK go where
    go a' = do
        a   <- request a'
        _   <- lift md
        a'2 <- respond a
        go a'2
{- execD md = foreverK $ \a' -> do
    a <- request a'
    lift md
    respond a -}
{-# INLINABLE execD #-}

{-| @(execU mu)@ executes @mu@ every time values flow upstream through it.

> execU mu1 >-> execU mu2 = execU (mu2 >> mu1)
>
> execU (return ()) = idPull
-}
execU :: (Monad m, Proxy p) => m r2 -> a' -> p a' a a' a m r
execU mu = runIdentityK go where
    go a' = do
        _   <- lift mu
        a   <- request a'
        a'2 <- respond a
        go a'2
{- execU mu = foreverK $ \a' -> do
    lift mu
    a <- request a'
    respond a -}
{-# INLINABLE execU #-}

{-| @(execB md mu)@ executes @mu@ every time values flow upstream through it,
    and executes @md@ every time values flow downstream through it.

> execB md1 mu1 >-> execB md2 mu2 = execB (md1 >> md2) (mu2 >> mu1)
>
> execB (return ()) = idPull
-}
execB :: (Monad m, Proxy p) => m r1 -> m r2 -> a' -> p a' a a' a m r
execB md mu = runIdentityK go where
    go a' = do
        _   <- lift mu
        a   <- request a'
        _   <- lift md
        a'2 <- respond a
        go a'2
{- execB md mu = foreverK $ \a' -> do
    lift mu
    a <- request a'
    lift md
    respond a -}
{-# INLINABLE execB #-}

{-| @(takeB n)@ allows @n@ upstream/downstream roundtrips to pass through

> takeB n1 >=> takeB n2 = takeB (n1 + n2)  -- n1 >= 0 && n2 >= 0
>
> takeB 0 = return
-}
takeB :: (Monad m, Proxy p) => Int -> a' -> p a' a a' a m a'
takeB n0 = runIdentityK (go n0) where
    go n
        | n <= 0    = return
        | otherwise = \a' -> do
             a   <- request a'
             a'2 <- respond a
             go (n - 1) a'2
-- takeB n = runIdentityK $ replicateK n $ request >=> respond
{-# INLINABLE takeB #-}

-- | 'takeB_' is 'takeB' with a @()@ return value, convenient for composing
takeB_ :: (Monad m, Proxy p) => Int -> a' -> p a' a a' a m ()
takeB_ n0 = runIdentityK (go n0) where
    go n
        | n <= 0    = \_ -> return ()
        | otherwise = \a' -> do
            a   <- request a'
            a'2 <- respond a
            go (n - 1) a'2
-- takeB_ n = fmap void (takeB n)
{-# INLINABLE takeB_ #-}

{-| @(takeWhileD p)@ allows values to pass downstream so long as they satisfy
    the predicate @p@.

> -- Using the "All" monoid over functions:
> mempty = \_ -> True
> (p1 <> p2) a = p1 a && p2 a
>
> takeWhileD p1 >-> takeWhileD p2 = takeWhileD (p1 <> p2)
>
> takeWhileD mempty = idPull
-}
takeWhileD :: (Monad m, Proxy p) => (a -> Bool) -> a' -> p a' a a' a m ()
takeWhileD p = runIdentityK go where
    go a' = do
        a <- request a'
        if (p a)
            then do
                a'2 <- respond a
                go a'2
            else return ()
{-# INLINABLE takeWhileD #-}

{-| @(takeWhileU p)@ allows values to pass upstream so long as they satisfy the
    predicate @p@.

> takeWhileU p1 >-> takeWhileU p2 = takeWhileU (p1 <> p2)
>
> takeWhileD mempty = idPull
-}
takeWhileU :: (Monad m, Proxy p) => (a' -> Bool) -> a' -> p a' a a' a m ()
takeWhileU p = runIdentityK go where
    go a' =
        if (p a')
            then do
                a   <- request a'
                a'2 <- respond a
                go a'2
            else return_P ()
{-# INLINABLE takeWhileU #-}

{-| @(dropD n)@ discards @n@ values going downstream

> dropD n1 >-> dropD n2 = dropD (n1 + n2)  -- n2 >= 0 && n2 >= 0
>
> dropD 0 = idPull
-}
dropD :: (Monad m, Proxy p) => Int -> () -> Pipe p a a m r
dropD n0 = \() -> runIdentityP (go n0) where
    go n
        | n <= 0    = idPull ()
        | otherwise = do
            _ <- request ()
            go (n - 1)
{- dropD n () = do
    replicateM_ n $ request ()
    idPull () -}
{-# INLINABLE dropD #-}

{-| @(dropU n)@ discards @n@ values going upstream

> dropU n1 >-> dropU n2 = dropU (n1 + n2)  -- n2 >= 0 && n2 >= 0
>
> dropU 0 = idPull
-}
dropU :: (Monad m, Proxy p) => Int -> a' -> CoPipe p a' a' m r
dropU n0 = runIdentityK (go n0) where
    go n
        | n <= 0    = idPull
        | otherwise = \_ -> do
            a' <- respond ()
            go (n - 1) a'
{-# INLINABLE dropU #-}

{-| @(dropWhileD p)@ discards values going downstream until one violates the
    predicate @p@.

> -- Using the "Any" monoid over functions:
> mempty = \_ -> False
> (p1 <> p2) a = p1 a || p2 a
>
> dropWhileD p1 >-> dropWhileD p2 = dropWhileD (p1 <> p2)
>
> dropWhileD mempty = idPull
-}
dropWhileD :: (Monad m, Proxy p) => (a -> Bool) -> () -> Pipe p a a m r
dropWhileD p () = runIdentityP go where
    go = do
        a <- request ()
        if (p a)
            then go
            else do
                x <- respond a
                idPull x
{-# INLINABLE dropWhileD #-}

{-| @(dropWhileU p)@ discards values going upstream until one violates the
    predicate @p@.

> dropWhileU p1 >-> dropWhileU p2 = dropWhileU (p1 <> p2)
>
> dropWhileU mempty = idPull
-}
dropWhileU :: (Monad m, Proxy p) => (a' -> Bool) -> a' -> CoPipe p a' a' m r
dropWhileU p = runIdentityK go where
    go a' =
        if (p a')
            then do
                a2 <- respond ()
                go a2
            else idPull a'
{-# INLINABLE dropWhileU #-}

{-| @(filterD p)@ discards values going downstream if they fail the predicate
    @p@

> -- Using the "All" monoid over functions:
> mempty = \_ -> True
> (p1 <> p2) a = p1 a && p2 a
>
> filterD p1 >-> filterD p2 = filterD (p1 <> p2)
>
> filterD mempty = idPull
-}
filterD :: (Monad m, Proxy p) => (a -> Bool) -> () -> Pipe p a a m r
filterD p = \() -> runIdentityP go where
    go = do
        a <- request ()
        if (p a)
            then do
                respond a
                go
            else go
{-# INLINABLE filterD #-}

{-| @(filterU p)@ discards values going upstream if they fail the predicate @p@

> filterU p1 >-> filterU p2 = filterU (p1 <> p2)
>
> filterU mempty = idPull
-}
filterU :: (Monad m, Proxy p) => (a' -> Bool) -> a' -> CoPipe p a' a' m r
filterU p = runIdentityK go where
    go a' =
        if (p a')
        then do
            _   <- request a'
            a'2 <- respond ()
            go a'2
        else do
            a'2 <- respond ()
            go a'2
{-# INLINABLE filterU #-}

{-| Convert a list into a 'Producer'

> fromListS xs >=> fromListS ys = fromListS (xs ++ ys)
>
> fromListS [] = return
-}
fromListS :: (Monad m, Proxy p) => [b] -> () -> Producer p b m ()
fromListS xs = \_ -> foldr (\e a -> respond e ?>= \_ -> a) (return_P ()) xs
-- fromListS xs _ = mapM_ respond xs
{-# INLINABLE fromListS #-}

{-| Convert a list into a 'CoProducer'

> fromListC xs >=> fromListC ys = fromListC (xs ++ ys)
>
> fromListC [] = return
-}
fromListC :: (Monad m, Proxy p) => [a'] -> () -> CoProducer p a' m ()
fromListC xs = \_ -> foldr (\e a -> request e ?>= \_ -> a) (return_P ()) xs
-- fromListC xs _ = mapM_ request xs
{-# INLINABLE fromListC #-}

-- | 'Producer' version of 'enumFrom'
enumFromS :: (Enum b, Monad m, Proxy p) => b -> () -> Producer p b m r
enumFromS b0 = \_ -> runIdentityP (go b0) where
    go b = do
        _ <- respond b
        go $! succ b
{-# INLINABLE enumFromS #-}

-- | 'CoProducer' version of 'enumFrom'
enumFromC :: (Enum a', Monad m, Proxy p) => a' -> () -> CoProducer p a' m r
enumFromC a'0 = \_ -> runIdentityP (go a'0) where
    go a' = do
        _ <- request a'
        go $! succ a'
{-# INLINABLE enumFromC #-}

-- | 'Producer' version of 'enumFromTo'
enumFromToS
    :: (Enum b, Ord b, Monad m, Proxy p) => b -> b -> () -> Producer p b m ()
enumFromToS b1 b2 _ = runIdentityP (go b1) where
    go b
        | b > b2    = return ()
        | otherwise = do
            _ <- respond b
            go $! succ b
{-# INLINABLE enumFromToS #-}

-- | 'CoProducer' version of 'enumFromTo'
enumFromToC
    :: (Enum a', Ord a', Monad m, Proxy p)
    => a' -> a' -> () -> CoProducer p a' m ()
enumFromToC a1 a2 _ = runIdentityP (go a1) where
    go n
        | n > a2 = return ()
        | otherwise = do
            _ <- request n
            go $! succ n
{-# INLINABLE enumFromToC #-}

{-| Non-deterministically choose from all values in the given list

> mappend <$> eachS xs <*> eachS ys = eachS (mappend <$> xs <*> ys)
>
> eachS (pure mempty) = pure mempty
-}
eachS :: (Monad m, Proxy p) => [b] -> ProduceT p m b
eachS bs = RespondT (fromListS bs ())
{-# INLINABLE eachS #-}

{-| Non-deterministically choose from all values in the given list

> mappend <$> eachC xs <*> eachC ys = eachC (mappend <$> xs <*> ys)
>
> eachC (pure mempty) = pure mempty
-}
eachC :: (Monad m, Proxy p) => [a'] -> CoProduceT p m a'
eachC a's = RequestT (fromListC a's ())
{-# INLINABLE eachC #-}

-- | Non-deterministically choose from all values in the given range
rangeS :: (Enum b, Ord b, Monad m, Proxy p) => b -> b -> ProduceT p m b
rangeS b1 b2 = RespondT (enumFromToS b1 b2 ())
{-# INLINABLE rangeS #-}

-- | Non-deterministically choose from all values in the given range
rangeC
    :: (Enum a', Ord a', Monad m, Proxy p) => a' -> a' -> CoProduceT p m a'
rangeC a'1 a'2 = RequestT (enumFromToC a'1 a'2 ())
{-# INLINABLE rangeC #-}

{-| Strict fold over values flowing \'@D@\'ownstream.

> foldD f >-> foldD g = foldD (f <> g)
>
> foldD mempty = idPull
-}
foldD
    :: (Monad m, Proxy p, Monoid w)
    => (a -> w) -> x -> WriterP w p x a x a m r
foldD f = go where
    go x = do
        a <- request x
        W.tell (f a)
        x2 <- respond a
        go x2
{-# INLINABLE foldD #-}

{-| Strict fold over values flowing \'@U@\'pstream.

> foldU f >-> foldU g = foldU (g <> f)
>
> foldU mempty = idPull
-}
foldU
    :: (Monad m, Proxy p, Monoid w)
    => (a' -> w) -> a' -> WriterP w p a' x a' x m r
foldU f = go where
    go a' = do
        W.tell (f a')
        x <- request a'
        a'2 <- respond x
        go a'2
{-# INLINABLE foldU #-}

{-| Fold that returns whether 'All' values flowing \'@D@\'ownstream satisfy the
    predicate
-}
allD :: (Monad m, Proxy p) => (a -> Bool) -> x -> WriterP All p x a x a m r
allD predicate = foldD (All . predicate)
{-# INLINABLE allD #-}

{-| Fold that returns whether 'All' values flowing \'@U@\'pstream satisfy the
    predicate
-}
allU :: (Monad m, Proxy p) => (a' -> Bool) -> a' -> WriterP All p a' x a' x m r
allU predicate = foldU (All . predicate)
{-# INLINABLE allU #-}

{-| Fold that returns whether 'All' values flowing \'@D@\'ownstream satisfy the
    predicate

    'allD_' terminates on the first value that fails the predicate
-}
allD_ :: (Monad m, Proxy p) => (a -> Bool) -> x -> WriterP All p x a x a m ()
allD_ predicate = go where
    go x = do
        a <- request x
        if (predicate a)
            then do
                x2 <- respond a
                go x2
            else W.tell (All False)
{-# INLINABLE allD_ #-}

{-| Fold that returns whether 'All' values flowing \'@U@\'pstream satisfy the
    predicate

    'allU_' terminates on the first value that fails the predicate
-}
allU_
    :: (Monad m, Proxy p)
    => (a' -> Bool) -> a' -> WriterP All p a' x a' x m ()
allU_ predicate = go where
    go a' =
        if (predicate a')
            then do
                x   <- request a'
                a'2 <- respond x
                go a'2
            else W.tell (All False)
{-# INLINABLE allU_ #-}

{-| Fold that returns whether 'Any' value flowing \'@D@\'ownstream satisfies
    the predicate
-}
anyD :: (Monad m, Proxy p) => (a -> Bool) -> x -> WriterP Any p x a x a m r
anyD predicate = foldD (Any . predicate)
{-# INLINABLE anyD #-}

{-| Fold that returns whether 'Any' value flowing \'@U@\'pstream satisfies
    the predicate
-}
anyU :: (Monad m, Proxy p) => (a' -> Bool) -> a' -> WriterP Any p a' x a' x m r
anyU predicate = foldU (Any . predicate)
{-# INLINABLE anyU #-}

{-| Fold that returns whether 'Any' value flowing \'@D@\'ownstream satisfies the
    predicate

    'anyD_' terminates on the first value that satisfies the predicate
-}
anyD_ :: (Monad m, Proxy p) => (a -> Bool) -> x -> WriterP Any p x a x a m ()
anyD_ predicate = go where
    go x = do
        a <- request x
        if (predicate a)
            then W.tell (Any True)
            else do
                x2 <- respond a
                go x2
{-# INLINABLE anyD_ #-}

{-| Fold that returns whether 'Any' value flowing \'@U@\'pstream satisfies the
    predicate

    'anyU_' terminates on the first value that satisfies the predicate
-}
anyU_
    :: (Monad m, Proxy p)
    => (a' -> Bool) -> a' -> WriterP Any p a' x a' x m ()
anyU_ predicate = go where
    go a' =
        if (predicate a')
            then W.tell (Any True)
            else do
                x   <- request a'
                a'2 <- respond x
                go a'2
{-# INLINABLE anyU_ #-}

-- | Compute the 'Sum' of all values that flow \'@D@\'ownstream
sumD :: (Monad m, Proxy p, Num a) => x -> WriterP (Sum a) p x a x a m r
sumD = foldD Sum
{-# INLINABLE sumD #-}

-- | Compute the 'Sum' of all values that flow \'@U@\'pstream
sumU :: (Monad m, Proxy p, Num a') => a' -> WriterP (Sum a') p a' x a' x m r
sumU = foldU Sum
{-# INLINABLE sumU #-}

-- | Compute the 'Product' of all values that flow \'@D@\'ownstream
productD
    :: (Monad m, Proxy p, Num a) => x -> WriterP (Product a) p x a x a m r
productD = foldD Product
{-# INLINABLE productD #-}

-- | Compute the 'Product' of all values that flow \'@U@\'pstream
productU
    :: (Monad m, Proxy p, Num a')
    => a' -> WriterP (Product a') p a' x a' x m r
productU = foldU Product
{-# INLINABLE productU #-}

-- | Count how many values flow \'@D@\'ownstream
lengthD :: (Monad m, Proxy p) => x -> WriterP (Sum Int) p x a x a m r
lengthD = foldD (\_ -> Sum 1)
{-# INLINABLE lengthD #-}

-- | Count how many values flow \'@U@\'pstream
lengthU :: (Monad m, Proxy p) => a' -> WriterP (Sum Int) p a' x a' x m r
lengthU = foldU (\_ -> Sum 1)
{-# INLINABLE lengthU #-}

-- | Retrieve the first value going \'@D@\'ownstream
headD :: (Monad m, Proxy p) => x -> WriterP (First a) p x a x a m r
headD = foldD (First . Just)
{-# INLINABLE headD #-}

{-| Retrieve the first value going \'@D@\'ownstream

    'headD_' terminates on the first value it receives
-}
headD_ :: (Monad m, Proxy p) => x -> WriterP (First a) p x a x a m ()
headD_ x = do
    a <- request x
    W.tell $ First (Just a)
{-# INLINABLE headD_ #-}

-- | Retrieve the first value going \'@U@\'pstream
headU :: (Monad m, Proxy p) => a' -> WriterP (First a') p a' x a' x m r
headU = foldU (First . Just)
{-# INLINABLE headU #-}

{-| Retrieve the first value going \'@U@\'pstream

    'headU_' terminates on the first value it receives
-}
headU_ :: (Monad m, Proxy p) => a' -> WriterP (First a') p a' x a' x m ()
headU_ a' = W.tell $ First (Just a')
{-# INLINABLE headU_ #-}

-- | Retrieve the last value going \'@D@\'ownstream
lastD :: (Monad m, Proxy p) => x -> WriterP (Last a) p x a x a m r
lastD = foldD (Last . Just)
{-# INLINABLE lastD #-}

-- | Retrieve the last value going \'@U@\'pstream
lastU :: (Monad m, Proxy p) => a' -> WriterP (Last a') p a' x a' x m r
lastU = foldU (Last . Just)
{-# INLINABLE lastU #-}

-- | Fold the values flowing \'@D@\'ownstream into a list
toListD :: (Monad m, Proxy p) => x -> WriterP [a] p x a x a m r
toListD = foldD (\x -> [x])
{-# INLINABLE toListD #-}

-- | Fold the values flowing \'@U@\'pstream into a list
toListU :: (Monad m, Proxy p) => a' -> WriterP [a'] p a' x a' x m r
toListU = foldU (\x -> [x])
{-# INLINABLE toListU #-}

{-| Fold equivalent to 'foldr'

    To see why, consider this isomorphic type for 'foldr':

> foldr :: (a -> b -> b) -> [a] -> Endo b
-}
foldrD
    :: (Monad m, Proxy p)
    => (a -> b -> b) -> x -> WriterP (Endo b) p x a x a m r
foldrD step = foldD (Endo . step)
{-# INLINABLE foldrD #-}

-- | Fold equivalent to 'foldr'
foldrU
    :: (Monad m, Proxy p)
    => (a' -> b -> b) -> a' -> WriterP (Endo b) p a' x a' x m r
foldrU step = foldU (Endo . step)
{-# INLINABLE foldrU #-}

{- $choice
    'leftD' and 'rightD' satisfy the 'ArrowChoice' laws using @arr = mapD@.

    'leftU' and 'rightU' satisfy the 'ArrowChoice' laws using @arr = mapU@.
-}

{-| Lift a proxy to operate only on 'Left' values flowing \'@D@\'ownstream and
    forward 'Right' values
-}
leftD
    :: (Monad m, Proxy p)
    => (q -> p x a x b m r) -> (q -> p x (Either a e) x (Either b e) m r)
leftD k = runIdentityK (up \>\ (identityK k />/ dn))
  where
    dn b = respond (Left b)
    up x = do
        ma <- request x
        case ma of
            Left  a -> return a
            Right e -> do
                x2 <- respond (Right e)
                up x2
{-# INLINABLE leftD #-}

{-| Lift a proxy to operate only on 'Right' values flowing \'@D@\'ownstream and
    forward 'Left' values
-}
rightD
    :: (Monad m, Proxy p)
    => (q -> p x a x b m r) -> (q -> p x (Either e a) x (Either e b) m r)
rightD k = runIdentityK (up \>\ (identityK k />/ dn))
  where
    dn b = respond (Right b)
    up x = do
        ma <- request x
        case ma of
            Left  e -> do
                x2 <- respond (Left e)
                up x2
            Right a -> return a
{-# INLINABLE rightD #-}

{-| Lift a proxy to operate only on 'Left' values flowing \'@U@\'pstream and
    forward 'Right' values
-}
leftU
    :: (Monad m, Proxy p)
    => (q -> p a' x b' x m r) -> (q -> p (Either a' e) x (Either b' e) x m r)
leftU k = runIdentityK ((up \>\ identityK k) />/ dn)
  where
    up a' = request (Left a')
    dn x = do
        mb' <- respond x
        case mb' of
            Left  b' -> return b'
            Right e  -> do
                x2 <- request (Right e)
                dn x2
{-# INLINABLE leftU #-}

{-| Lift a proxy to operate only on 'Right' values flowing \'@D@\'ownstream and
    forward 'Left' values
-}
rightU
    :: (Monad m, Proxy p)
    => (q -> p a' x b' x m r) -> (q -> p (Either e a') x (Either e b') x m r)
rightU k = runIdentityK ((up \>\ identityK k) />/ dn)
  where
    up a' = request (Right a')
    dn x = do
        mb' <- respond x
        case mb' of
            Left  e  -> do
                x2 <- request (Left e)
                dn x2
            Right b' -> return b'
{-# INLINABLE rightU #-}

-- | Zip values flowing downstream
zipD
    :: (Monad m, Proxy p1, Proxy p2, Proxy p3)
    => () -> Consumer p1 a (Consumer p2 b (Producer p3 (a, b) m)) r
zipD () = runIdentityP $ hoist (runIdentityP . hoist runIdentityP) go where
    go = do
        a <- request ()
        lift $ do
            b <- request ()
            lift $ respond (a, b)
        go
{-# INLINABLE zipD #-}

-- | Interleave values flowing downstream using simple alternation
mergeD
    :: (Monad m, Proxy p1, Proxy p2, Proxy p3)
    => () -> Consumer p1 a (Consumer p2 a (Producer p3 a m)) r
mergeD () = runIdentityP $ hoist (runIdentityP . hoist runIdentityP) go where
    go = do
        a1 <- request ()
        lift $ do
            lift $ respond a1
            a2 <- request ()
            lift $ respond a2
        go
{-# INLINABLE mergeD #-}

{- $open
    Use the @unit@ functions when you need to embed a proxy with a closed end
    within an open proxy.  For example, the following code will not type-check
    because @fromListS [1..]@  is a 'Producer' and has a closed upstream end,
    which conflicts with the 'request' statement preceding it:

> p () = do
>     request ()
>     fromList [1..] ()

    You fix this by composing 'unitD' upstream of it, which replaces its closed
    upstream end with an open polymorphic end:

> p () = do
>     request ()
>     (fromList [1..] <-< unitD) ()

-}

-- | Compose 'unitD' with a closed upstream end to create a polymorphic end
unitD :: (Monad m, Proxy p) => q -> p x' x y' () m r
unitD _ = runIdentityP go where
    go = do
        _ <- respond ()
        go
{-# INLINABLE unitD #-}

-- | Compose 'unitU' with a closed downstream end to create a polymorphic end
unitU :: (Monad m, Proxy p) => q -> p () x y' y m r
unitU _ = runIdentityP go where
    go = do
        _ <- request ()
        go
{-# INLINABLE unitU #-}

{- $modules
    These modules help you build, run, and extract folds
-}
