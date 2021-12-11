module Main where

import qualified PinBoard as P
import System.Mem
import Data.IORef
import System.Mem.Weak
import Data.Maybe
import Control.Concurrent.MVar
import qualified Data.IntMap as I
import Data.Foldable

main :: IO ()
main = do
  foo 5

-- foo :: Int -> IO ()
-- foo n = do
--   let x = ("hello, world! " ++ show n)
--   b <- newMVar I.empty
--   _ <- P.pin b x
--   readMVar b >>= printWeaks >>= print
--   putStrLn ("I kept it alive: " ++ x)
--   performGC
--   putStrLn "It should be dead"
--   readMVar b >>= printWeaks >>= print
--   where
--     printWeaks :: I.IntMap [Weak String] -> IO [String]
--     printWeaks ws = do
--       vs <- traverse deRefWeak (concat (toList ws))
--       pure (catMaybes vs)

foo :: Int -> IO ()
foo n = do
  b <- bar n
  P.toList b >>= print
  performGC
  putStrLn "It should be dead"
  myToList b >>= print
{-# NOINLINE foo #-}

bar :: Int -> IO (MVar (I.IntMap [Weak String]))
bar n = do
  let x = ("hello, world! " ++ show n)
  b <- P.new
  P.pin b x
  pure b
{-# NOINLINE bar #-}

myToList :: MVar (I.IntMap [Weak String]) -> IO [String]
myToList boardVar = do
  board <- readMVar boardVar
  catMaybes <$> traverse deRefWeak (concat (toList board))
