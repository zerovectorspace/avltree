{-
  This file is part of AVLTree.

  fct is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  fct is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with fct.  If not, see <http://www.gnu.org/licenses/>.

  Copyright 2018 Zachary Young
  -}

module AVLTree ( AVLTree ( X, T )
            , add
            , build
            , fold
            , get
            , getM
            , getOr
            , height
            , inOrder
            , join
            , keys
            , levelOrder
            , map
            , max
            , min
            , postOrder
            , preOrder
            , print
            , size
            , toList
            , toListR
            , toListL
            , vals
            , upd
            ) where

import Prelude hiding (map, max, min, mapM_)
import Data.Maybe
import Data.List hiding (map, rm)
import Data.Char

type Balance = Int
data AVLTree k v = X
                 | T { pr :: (k, v)
                     , lt :: (AVLTree k v)
                     , rt :: (AVLTree k v)
                     } deriving (Read, Show)

instance Functor (AVLTree a) where
  fmap = map

add :: (Ord a) => (a, b) -> AVLTree a b -> AVLTree a b
add kv@(nk,_) X = T kv X X
add nkv@(nk,_) (T kv@(k,v) l r)
  | nk < k && balL == -2 && nk < (key l) = balLL $ T kv (add nkv l) r
  | nk < k && balL == -2 && nk > (key l) = balLR $ T kv (add nkv l) r
  
  | nk > k && balR == 2 && nk > (key r)  = balRR $ T kv l (add nkv r)
  | nk > k && balR == 2 && nk < (key r)  = balRL $ T kv l (add nkv r)

  | nk < k = T kv (add nkv l) r
  | nk > k = T kv l (add nkv r)

  | otherwise = T kv l r
  where
  balL = bal $ T kv (add nkv l) r
  balR = bal $ T kv l (add nkv r)
  key = fst . pr

bal :: AVLTree a b -> Int
bal (T _ l r) = (height r) - (height l)

bal' :: AVLTree a b -> AVLTree a b -> Int
bal' l r = (height r) - (height l)

balLL :: AVLTree a b -> AVLTree a b
balLL (T kv (T pkv pl pr) r)                   = (T pkv pl (T kv pr r))
balRR :: AVLTree a b -> AVLTree a b
balRR (T kv l (T pkv pl pr))                   = (T pkv (T kv l pl) pr)

balLR :: AVLTree a b -> AVLTree a b
balLR (T kv (T pkv pl (T prkv prl prr)) r) = (T prkv (T pkv pl prl) (T kv prr r))
balRL :: AVLTree a b -> AVLTree a b
balRL (T kv l (T pkv (T plv pll plr) pr))  = (T plv (T kv l pll) (T pkv plr pr))

build :: (Ord a) => [(a,b)] -> AVLTree a b -> AVLTree a b
build [] t = t
build (kv:kvs) t = (build kvs . add kv) t

fold :: (b -> b -> b) -> b -> AVLTree a b -> b
fold f x X = x
fold f x (T (_,v) l r) = 
  f x (fold f (fold f v l) r)

getM :: (Ord a) => a -> AVLTree a b -> Maybe b
getM x X      = Nothing
getM x (T (k,v) l r)
  | x < k     = getM x l
  | x > k     = getM x r
  | otherwise = Just v

get :: (Ord a) => a -> AVLTree a b -> b
get x X    = error "Value is not in AVLTree"
get x (T (k,v) l r)
  | x < k     = get x l
  | x > k     = get x r
  | otherwise = v

getOr :: (Ord a) => a -> b -> AVLTree a b -> b
getOr x dflt X = dflt
getOr x dflt (T (k,v) l r)
  | x < k     = getOr x dflt l
  | x > k     = getOr x dflt r
  | otherwise = v

height :: AVLTree a b -> Int
height X = (-1)
height (T kv l r) = (max' (height l) (height r)) + 1

inOrder :: AVLTree a b -> [AVLTree a b]
inOrder X = []
inOrder t@(T (k,v) l r) = inOrder l ++ [t] ++ inOrder r

join :: AVLTree a b -> AVLTree a b -> AVLTree a b
join X tr = tr
join (T kv l r) tr = T kv (join l tr) r

keys :: AVLTree a b -> [a]
keys X = []
keys (T (k,_) l r) = [k] ++ keys l ++ keys r

levelOrder :: AVLTree a b -> [AVLTree a b]
levelOrder t = go [t] []
  where
    go [] ret = ret
    go (X:xs) ret = go xs (ret ++ [X])
    go (tree@(T (k,v) l r):xs) ret = go (xs ++ [l] ++ [r]) (ret ++ [tree])

map :: (b -> c) -> AVLTree a b -> AVLTree a c
map f X = X
map f (T (k,v) l r) = T (k,f v) (map f l) (map f r)

max :: AVLTree a b -> (a,b)
max (T kv _ X) = kv
max (T _ _ r)    = max r

max' :: (Ord a) => a -> a -> a
max' x y = if x >= y then x else y

min :: AVLTree a b -> (a,b)
min (T kv X _) = kv
min (T _ l _)    = min l

postOrder :: AVLTree a b -> [AVLTree a b]
postOrder X = []
postOrder t@(T (k,v) l r) = postOrder l ++ postOrder r ++ [t]

printT :: (Show a, Show b) => AVLTree a b -> IO()
printT X = return ()
printT (T (k,v) l r)
  =  printT l
  >> print (k,v)
  >> printT r

preOrder :: AVLTree a b -> [AVLTree a b]
preOrder X = []
preOrder t@(T (k,v) l r) = [t] ++ preOrder l ++ preOrder r

reverseOrder :: AVLTree a b -> [AVLTree a b]
reverseOrder X = []
reverseOrder t@(T (k,v) l r) = reverseOrder r ++ [t] ++ reverseOrder l

rm :: (Ord a) => AVLTree a b -> a -> AVLTree a b
rm X d = X
rm (T kv@(k,v) X X) d = if d == k then X else (T kv X X)
rm (T kv@(k,v) l X) d = if d == k then l else (T kv l X)
rm (T kv@(k,v) X r) d = if d == k then r else (T kv X r)
rm (T kv@(k,v) l r) d
    | d == k                                = (T mr l dmin)

    | d > k && (bal' (lt l) (rt l)) == (-1) = balLL (T kv l dr)
    | d < k && (bal' (lt r) (rt r)) == 1    = balRR (T kv dl r)

    | d > k && nbr == (-2)                  = balLR (T kv l dr)
    | d < k && nbl == 2                     = balRL (T kv dl r)

    | d > k                                 = (T kv l dr)
    | d < k                                 = (T kv dl r)

        where dmin = rm r (fst mr)
              dl   = rm l d
              dr   = rm r d
              mr   = min r
              nbl  = bal' dl r
              nbr  = bal' l dr

size :: AVLTree a b -> Int
size X           = 0
size (T _ l r) = 1 + size l + size r

toList :: AVLTree a b -> [(a,b)]
toList X           = []
toList (T (k,v) l r) = toList l ++ [(k,v)] ++ toList r

toListR :: AVLTree a b -> [(a,b)]
toListR X           = []
toListR (T (k,v) l r) = toListR r ++ [(k,v)] ++ toListR l

toListL :: AVLTree a b -> [(a,b)]
toListL t = go [t] []
  where
    go [] ret = ret
    go (X:xs) ret = go xs ret
    go ((T kv l r):xs) ret = go (xs ++ [l] ++ [r]) (ret ++ [kv])

upd :: (Ord a) => ( b -> b -> b ) -> (a, b) -> AVLTree a b -> AVLTree a b
upd f kv@(nk,_) X = X
upd f nkv@(nk,nv) (T kv@(k,v) l r)
  | nk < k = T kv (upd f nkv l) r
  | nk > k = T kv l (upd f nkv r)
  | otherwise = T (k, f v nv) l r
  where
  balL = bal $ T kv (upd f nkv l) r
  balR = bal $ T kv l (upd f nkv r)

vals :: AVLTree a b -> [b]
vals X = []
vals (T (_,v) l r) = [v] ++ vals l ++ vals r
