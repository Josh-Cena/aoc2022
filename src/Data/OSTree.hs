-- Copied from https://github.com/shlok/ostree/blob/master/src/Data/OSTree.hs.
-- Please see https://github.com/lambdakazan/ostree/blob/master/LICENSE for license
-- and https://hackage.haskell.org/package/order-statistic-tree for the package.
-- Copied because the package doesn't have `rank`: https://github.com/lambdakazan/ostree/issues/2
-- Also changed to zero-based indexing
module Data.OSTree
  ( OSTree,

    -- * Creating OSTree
    empty,
    singleton,

    -- * Search Tree operations
    size,
    insert,
    lookup,
    delete,

    -- * Conversions
    toList,
    fromList,

    -- * Statistics
    select,

    -- * Rank
    rank,
  )
where

import Data.List (foldl')
import Prelude hiding (lookup)

-- https://yoichihirai.com/bst.pdf

type Size = Int

-- | Order statistic tree with elements of type 'a'
data OSTree a
  = Tip
  | Bin {-# UNPACK #-} !Size !a !(OSTree a) !(OSTree a)
  deriving (Eq, Show)

-- | /O(1)/. Returns an empty tree
empty :: OSTree a
empty = Tip

-- | /O(1)/. Returns a tree with single element
singleton :: a -> OSTree a
singleton k = Bin 1 k Tip Tip

-- | /O(log n)/. Insert the element into the tree
insert :: (Ord a) => a -> OSTree a -> OSTree a
insert kx Tip = singleton kx
insert kx (Bin _ ky l r) = case compare kx ky of
  LT -> balanceR ky (insert kx l) r
  GT -> balanceL ky l (insert kx r)
  EQ -> balanceR ky l (insert kx r)

-- | /O(log n)/. Lookup the element in the tree
lookup :: (Ord a) => a -> OSTree a -> Maybe a
lookup _ Tip = Nothing
lookup kx (Bin _ ky l r) = case compare kx ky of
  LT -> lookup kx l
  GT -> lookup kx r
  EQ -> Just ky

-- | /O(log n)/. Delete first occurrence of the element from the tree
delete :: (Ord a) => a -> OSTree a -> OSTree a
delete k t =
  case t of
    Tip -> Tip
    Bin _ kx l r ->
      case compare k kx of
        LT -> balanceL kx (delete k l) r
        GT -> balanceR kx l (delete k r)
        EQ -> glue l r

-- | /O(n)/. Return list of elements of the tree
toList :: OSTree a -> [a]
toList k = toListL k []

toListL :: OSTree a -> [a] -> [a]
toListL Tip = id
toListL (Bin _ k l r) = toListL l . (k :) . toListL r

-- | /O(n log n)/. Convert list of elements to the tree
fromList :: (Ord a) => [a] -> OSTree a
fromList = foldl' (flip insert) empty

-- | /O(log n)/. Returns i-th least element of the tree
select ::
  -- | tree
  OSTree a ->
  -- | index 'i', starting from 0
  Int ->
  -- | if there are at least 'i' elements, returns i-th least element
  Maybe a
select Tip _ = Nothing
select (Bin _ k l r) i =
  let n = size l
   in case compare i n of
        EQ -> Just k
        LT -> select l i
        GT -> select r $ i - (n + 1)

-- | /O(log n)/. If the given element is in the tree, returns its rank; returns 'Nothing' otherwise.
rank ::
  (Ord a) =>
  OSTree a ->
  -- | A given element.
  a ->
  Maybe Int
rank t a =
  go 0 False t
  where
    -- + sum': Number of elements already (in earlier iterations) identified to be less than the
    --   given element a.
    -- + found': Whether the given element a was already found (in earlier iterations).
    go !sum' !found' x = case x of
      Tip ->
        if found'
          then Just sum'
          else Nothing
      Bin _ k l r ->
        case compare a k of
          EQ ->
            -- The given element a is equal to k (and potentially equal to other elements in l).
            go sum' True l
          LT ->
            -- The given element a is less than k (and all elements in r).
            go sum' found' l
          GT ->
            -- The given element a is greater than k (and all elements in l).
            go (sum' + size l + 1) found' r

-- | Returns size of the tree
size :: OSTree a -> Size
size Tip = 0
size (Bin sz _ _ _) = sz

bin :: a -> OSTree a -> OSTree a -> OSTree a
bin k l r = Bin (size l + size r + 1) k l r

-- balances

balanceL :: a -> OSTree a -> OSTree a -> OSTree a
balanceL k l r
  | isBalanced l r = bin k l r
  | otherwise = rotateL k l r

balanceR :: a -> OSTree a -> OSTree a -> OSTree a
balanceR k l r
  | isBalanced r l = bin k l r
  | otherwise = rotateR k l r

-- rotation L

rotateL :: a -> OSTree a -> OSTree a -> OSTree a
rotateL k l r@(Bin _ _ rl rr)
  | isSingle rl rr = singleL k l r
  | otherwise = doubleL k l r
rotateL _ _ _ = error "rotateL"

singleL :: a -> OSTree a -> OSTree a -> OSTree a
singleL k1 t1 (Bin _ k2 t2 t3) = bin k2 (bin k1 t1 t2) t3
singleL _ _ _ = error "singleL"

doubleL :: a -> OSTree a -> OSTree a -> OSTree a
doubleL k1 t1 (Bin _ k2 (Bin _ k3 t2 t3) t4) =
  bin k3 (bin k1 t1 t2) (bin k2 t3 t4)
doubleL _ _ _ = error "doubleL"

-- rotation R

rotateR :: a -> OSTree a -> OSTree a -> OSTree a
rotateR k l@(Bin _ _ ll lr) r
  | isSingle lr ll = singleR k l r
  | otherwise = doubleR k l r
rotateR _ _ _ = error "rotateR"

singleR :: a -> OSTree a -> OSTree a -> OSTree a
singleR k1 (Bin _ k2 t1 t2) t3 = bin k2 t1 (bin k1 t2 t3)
singleR _ _ _ = error "singleR"

doubleR :: a -> OSTree a -> OSTree a -> OSTree a
doubleR k1 (Bin _ k2 t1 (Bin _ k3 t2 t3)) t4 =
  bin k3 (bin k2 t1 t2) (bin k1 t3 t4)
doubleR _ _ _ = error "doubleR"

-- config

isBalanced :: OSTree a -> OSTree a -> Bool
isBalanced a b = 3 * (size a + 1) >= size b + 1

isSingle :: OSTree a -> OSTree a -> Bool
isSingle a b = size a + 1 < 2 * (size b + 1)

-- | /O(log n)/. Delete and find the minimal element.
--
-- > deleteFindMin (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((3,"b"), fromList[(5,"a"), (10,"c")])
-- > deleteFindMin                                            Error: can not return the minimal element of an empty map
deleteFindMin :: OSTree a -> (a, OSTree a)
deleteFindMin t =
  case t of
    Bin _ k Tip r -> (k, r)
    Bin _ k l r -> let (km, l') = deleteFindMin l in (km, balance k l' r)
    Tip -> (error "Map.deleteFindMin: can not return the minimal element of an empty map", Tip)

-- | /O(log n)/. Delete and find the maximal element.
--
-- > deleteFindMax (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((10,"c"), fromList [(3,"b"), (5,"a")])
-- > deleteFindMax empty                                      Error: can not return the maximal element of an empty map
deleteFindMax :: OSTree a -> (a, OSTree a)
deleteFindMax t =
  case t of
    Bin _ k l Tip -> (k, l)
    Bin _ k l r -> let (km, r') = deleteFindMax r in (km, balance k l r')
    Tip -> (error "Map.deleteFindMax: can not return the maximal element of an empty map", Tip)

glue :: OSTree a -> OSTree a -> OSTree a
glue Tip r = r
glue l Tip = l
glue l r
  | size l > size r = let (km, l') = deleteFindMax l in balanceL km l' r
  | otherwise = let (km, r') = deleteFindMin r in balanceR km l r'

balance :: a -> OSTree a -> OSTree a -> OSTree a
balance k l r
  | isBalanced l r && isBalanced r l = bin k l r
  | size l > size r = rotateR k l r
  | otherwise = rotateL k l r
