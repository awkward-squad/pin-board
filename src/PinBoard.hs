-- | A utility type for saving memory in the presence of many duplicate ByteStrings, etc. If you have data that may be
-- a redundant duplicate, try pinning it to a pin board, and use the result of that operation instead.
--
-- Without a pin board:
--
-- @
--     x ───── "38dce848c8c829c62"
--     y ───── "38dce848c8c829c62"
--     z ───── "d2518f260535b927b"
-- @
--
-- With a pin board:
--
-- @
--     x ───── "38dce848c8c829c62" ┄┄┄┄┄┐
--     y ────────┘                     board
--     z ───── "d2518f260535b927b" ┄┄┄┄┄┘
-- @
--
--   ... and after @x@ is garbage collected:
--
-- @
--             "38dce848c8c829c62" ┄┄┄┄┄┐
--     y ────────┘                     board
--     z ───── "d2518f260535b927b" ┄┄┄┄┄┘
-- @
--
--   ... and after @y@ is garbage collected:
--
-- @
--                                     board
--     z ───── "d2518f260535b927b" ┄┄┄┄┄┘
-- @
module PinBoard
  ( PinBoard,
    new,
    pin,
    toList,
  )
where

import Control.Concurrent.MVar
import Control.Exception (mask_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (find)
import Data.Functor.Compose
import Data.Hashable (Hashable, hash)
import qualified Data.IntMap as IntMap
import Data.IntMap.Strict (IntMap)
import System.Mem.Weak (Weak, deRefWeak, mkWeakPtr)
import Prelude

-- | A "pin board" is a place to pin values; semantically, it's a set, but differs in a few ways:
--
--   * Pinned values aren't kept alive by the pin board, they might be garbage collected at any time.
--   * If you try to pin a value that's already pinned (per its Eq instance), the pinned one will be returned
--     instead.
newtype PinBoard a
  = PinBoard (MVar (IntMap (Bucket a)))

new :: forall a m. MonadIO m => m (PinBoard a)
new =
  liftIO (PinBoard <$> newMVar IntMap.empty)

pin :: forall a m. (Eq a, Hashable a, MonadIO m) => PinBoard a -> a -> m a
pin (PinBoard boardVar) x = liftIO do
  mask_ do
    board0 <- takeMVar boardVar
    case IntMap.lookup n board0 of
      -- Pin a new value: create a new singleton bucket.
      Nothing -> do
        bucket <- newBucket x finalizer
        putMVar boardVar $! IntMap.insert n bucket board0
        pure x
      Just bucket0 ->
        bucketFind bucket0 x >>= \case
          -- Hash collision: the bucket has things in it, but none are the given value. Insert.
          Nothing -> do
            bucket1 <- bucketAdd bucket0 x finalizer
            putMVar boardVar $! IntMap.insert n bucket1 board0
            pure x
          -- The thing being inserted already exists; return it.
          Just y -> do
            putMVar boardVar board0
            pure y
  where
    n :: Int
    n =
      hash x
    finalizer :: IO ()
    finalizer = do
      mask_ do
        board0 <- takeMVar boardVar
        board1 <- IntMap.alterF (maybe (pure Nothing) bucketCompact_) n board0
        putMVar boardVar $! board1

toList :: MonadIO m => PinBoard a -> m [a]
toList (PinBoard boardVar) = liftIO do
  mask_ do
    board0 <- takeMVar boardVar
    (values, board1) <- getCompose (IntMap.traverseMaybeWithKey (\_ -> Compose . bucketCompact) board0)
    putMVar boardVar $! board1
    pure values

-- A bucket of weak pointers to different values that all share a hash.
newtype Bucket a
  = Bucket [Weak a] -- Invariant: non-empty list

-- A singleton bucket.
newBucket :: a -> IO () -> IO (Bucket a)
newBucket =
  bucketAdd (Bucket [])

-- Add a value to a bucket.
bucketAdd :: Bucket a -> a -> IO () -> IO (Bucket a)
bucketAdd (Bucket weaks) x finalizer = do
  weak <- mkWeakPtr x (Just finalizer)
  pure (Bucket (weak : weaks))

-- Drop all garbage-collected values from a bucket. If none remain, returns Nothing.
bucketCompact :: Bucket a -> IO ([a], Maybe (Bucket a))
bucketCompact (Bucket weaks) = do
  alive <- compactWeaks (,) weaks
  let (ws, vs) = unzip alive
  pure (vs, bucketFromList ws)

-- Like bucketCompact, but doesn't return the alive values.
bucketCompact_ :: Bucket a -> IO (Maybe (Bucket a))
bucketCompact_ (Bucket weaks) =
  -- bucketFromList <$> compactWeaks const weaks
  bucketFromList <$> mapMaybeM (\w -> (w <$) <$> deRefWeak w) weaks

-- Look up a value in a bucket per its Eq instance.
bucketFind :: Eq a => Bucket a -> a -> IO (Maybe a)
bucketFind bucket x =
  find (== x) <$> bucketToList bucket

bucketFromList :: [Weak a] -> Maybe (Bucket a)
bucketFromList = \case
  [] -> Nothing
  weaks -> Just (Bucket weaks)

bucketToList :: Bucket a -> IO [a]
bucketToList (Bucket weaks) =
  mapMaybeM deRefWeak weaks

compactWeaks :: (Weak a -> a -> f b) -> [Weak a] -> IO [f b]
compactWeaks f =
  mapMaybeM \w -> fmap (f w) <$> deRefWeak w

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f =
  foldr step (pure [])
  where
    step x xs =
      f x >>= \case
        Nothing -> xs
        Just y -> (y :) <$> xs
