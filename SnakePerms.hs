{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}

module SnakePerms (RTree(..), simpleSnakePerms) where

import qualified Data.Set as S

data RTree a = a :> [RTree a] deriving (Show, Eq, Functor, Foldable, Traversable)

-- Path tree for a snake that grows forever on a field of size (w,h),
-- starting at pos (x,y).
simpleSnakePerms :: (Int, Int) -> (Int, Int) -> RTree (Int, Int)
simpleSnakePerms size pos = go size pos S.empty
  where
    go (w,h) (x,y) state =
      let state' = (x,y) `S.insert` state
          moves = [p | p@(i,j) <- [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
                     , i >= 0, i < w
                     , j >= 0, j < h
                     , p `S.notMember` state']
      in
       (x,y) :> fmap (\p -> go (w,h) p state') moves

-- > simpleSnakePerms (2,2) (0,0)
-- (0,0) :> [(1,0) :> [(1,1) :> [(0,1) :> []]],(0,1) :> [(1,1) :> [(1,0) :> []]]]
