-- | A utility type for saving memory in the presence of many duplicate ByteStrings, etc. If you have data that may be
-- a redundant duplicate, try pinning it to a pin board, and use the result of that operation instead.
--
--   Without a pin board:
--
--     x ───── "38dce848c8c829c62"
--     y ───── "38dce848c8c829c62"
--     z ───── "d2518f260535b927b"
--
--   With a pin board:
--
--     x ───── "38dce848c8c829c62" ┄┄┄┄┄┐
--     y ────────┘                     board
--     z ───── "d2518f260535b927b" ┄┄┄┄┄┘
--
--   ... and after x is garbage collected:
--
--             "38dce848c8c829c62" ┄┄┄┄┄┐
--     y ────────┘                     board
--     z ───── "d2518f260535b927b" ┄┄┄┄┄┘
--
--   ... and after y is garbage collected:
--
--                                     board
--     z ───── "d2518f260535b927b" ┄┄┄┄┄┘
module PinBoard
  ( PinBoard,
    new,
    pin,
  )
where

import Control.Concurrent.MVar
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (find)
import Data.Functor.Compose
import Data.Hashable (Hashable, hash)
import qualified Data.IntMap as IntMap
import Data.IntMap.Strict (IntMap)
import Data.Tuple (swap)
import System.Mem.Weak (Weak, deRefWeak, mkWeakPtr)
import Prelude

-- | A "pin board" is a place to pin values; semantically, it's a set, but differs in a few ways:
--
--   * Pinned values aren't kept alive by the pin board, they might be garbage collected at any time.
--   * If you try to pin a value that's already pinned (per its Eq instance), the pinned one will be returned
--     instead.
--   * It has a small API: just 'new' and 'pin'.
newtype PinBoard a
  = PinBoard (MVar (IntMap (Bucket a)))

new :: MonadIO m => m (PinBoard a)
new =
  liftIO (PinBoard <$> newMVar IntMap.empty)

pin :: forall a m. (Eq a, Hashable a, MonadIO m) => PinBoard a -> a -> m a
pin (PinBoard boardVar) x = liftIO do
  modifyMVar boardVar \board ->
    swap <$> getCompose (IntMap.alterF alter n board)
  where
    -- Pin to pin board at a hash key: either there's nothing there (ifMiss), or there's a nonempty bucket (ifHit).
    alter :: Maybe (Bucket a) -> Compose IO ((,) a) (Maybe (Bucket a))
    alter =
      Compose . maybe ifMiss ifHit
    -- Pin a new value: create a new singleton bucket.
    ifMiss :: IO (a, Maybe (Bucket a))
    ifMiss =
      (x,) . Just <$> newBucket x finalizer
    -- Possibly pin a new value: if it already exists in the bucket, return that one instead. Otherwise, insert it.
    ifHit :: Bucket a -> IO (a, Maybe (Bucket a))
    ifHit bucket =
      bucketFind bucket x >>= \case
        -- Hash collision: the bucket has things in it, but none are the given value. Insert.
        Nothing -> (x,) . Just <$> bucketAdd bucket x finalizer
        -- The thing being inserted already exists; return it.
        Just y -> pure (y, Just bucket)
    -- When each thing pinned here is garbage collected, compact its bucket.
    finalizer :: IO ()
    finalizer =
      modifyMVar_ boardVar (IntMap.alterF (maybe (pure Nothing) bucketCompact) n)
    n :: Int
    n =
      hash x

-- | A bucket of weak pointers to different values that all share a hash.
newtype Bucket a
  = Bucket [Weak a] -- Invariant: non-empty list

-- | A singleton bucket.
newBucket :: a -> IO () -> IO (Bucket a)
newBucket =
  bucketAdd (Bucket [])

-- | Add a value to a bucket.
bucketAdd :: Bucket a -> a -> IO () -> IO (Bucket a)
bucketAdd (Bucket weaks) x finalizer = do
  weak <- mkWeakPtr x (Just finalizer)
  pure (Bucket (weak : weaks))

-- | Drop all garbage-collected values from a bucket. If none remain, returns Nothing.
bucketCompact :: Bucket a -> IO (Maybe (Bucket a))
bucketCompact (Bucket weaks) =
  bucketFromList <$> mapMaybeM (\w -> (w <$) <$> deRefWeak w) weaks

-- | Look up a value in a bucket per its Eq instance.
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

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f =
  foldr step (pure [])
  where
    step x xs =
      f x >>= \case
        Nothing -> xs
        Just y -> (y :) <$> xs
