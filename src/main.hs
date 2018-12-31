{-
  This file is part of AVLTree.

  AVLTree is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  AVLTree is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with AVLTree.  If not, see <http://www.gnu.org/licenses/>.

  Copyright 2018 Zachary Young
  -}

module Main where

import qualified AVLTree as T
import System.Random
import Data.Time.Clock.POSIX
import Data.List.Split
import Data.Char

rMin = 1                                 -- Lower bound on random Int
rMax = 100                               -- Upper bound on random Int
num = 500                                 -- Number of branches
bounds = (+rMin) . (`mod` (rMax-rMin+1)) -- [rMin,rMax] i.e. inclusive

buildTree :: [(String,[Int])] -> T.AVLTree String [Int]
{- buildTree = T.map (filter even) -- Keep only even numbers -}
  {- . foldl (flip T.add) T.X      -- Apply T.add to create Tree -}

buildTree = foldl (flip T.add) T.X      -- Apply T.add to create Tree

kRnds :: POSIXTime -> [[Char]]
kRnds = take num                              -- Take finite amount
      . map ( \ns -> map (chr . (+0x61)) ns ) -- [[Int]] -> [[Char]]
      . chunksOf 3                            -- Make [[Int]]
      . map ( \x -> x `mod` (25-0-1) + 0   )  -- Constrain Int between [0,25]
      . rnds

vRnds :: POSIXTime -> [[Int]]
vRnds = take num    -- Take finite amount
      . chunksOf 10 -- Make [[Int]]
      . map bounds  -- Constrain the values
      . rnds

rnds :: POSIXTime -> [Int]
rnds = (randoms :: StdGen -> [Int])
     . mkStdGen -- Integer -> StdGen
     . round    -- POSIXTime -> Integer

main :: IO ()
main = do
  getPOSIXTime
  >>= \t ->
    print
    {- . T.mapKey (\k -> k ++ "xxx")  -}
    . buildTree
    $ zip (kRnds t) (vRnds t)

