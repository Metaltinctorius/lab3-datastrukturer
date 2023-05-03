  {-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------

module AATree (
  AATree,        -- type of AA search trees
  emptyTree,     -- AATree a
  get,           -- Ord a => a -> AATree a -> Maybe a
  insert,        -- Ord a => a -> AATree a -> AATree a
  inorder,       -- AATree a -> [a]
  remove,        -- Ord a => a -> AATree a -> AATree a
  size,          -- AATree a -> Int
  height,        -- AATree a -> Int
  checkTree      -- Ord a => AATree a -> Bool
 ) where

import qualified Data.Tree as T
--------------------------------------------------------------------------------

-- AA trees, represented by two constructors, Empty and Node
-- Node has three arguments, a left subtree, value a, and a right subtree
-- AA tree represented as BST tree
data AATree a
  = Empty
  | Node Int (AATree a) a (AATree a)
  deriving (Eq, Show, Read)

emptyTree :: AATree a
emptyTree = Empty

get :: Ord a => a -> AATree a -> Maybe a
get _ Empty = Nothing
get value (Node _ leftChild nodeValue rightChild)
  | value     == nodeValue = Just nodeValue
  | value     < nodeValue = get value leftChild
  | otherwise = get value rightChild

--testTree :: AATree a -> AATree a
--testTree = Node 2 Empty Empty

-- guard to make sure to pattern match only when the insertion created
-- a four-node, that is, when x y and z has the same level
split :: AATree a -> AATree a
split (Node xlvl a x (Node ylvl b y (Node zlvl c z d)))
  |ylvl == xlvl && xlvl == zlvl =  Node (ylvl+1) (Node xlvl a x b) y (Node zlvl c z d)
split tree = tree

-- guard case to ascertain that skew is only applied when a left child
-- is added and has the same height as its parent
skew :: AATree a -> AATree a
skew (Node ylvl (Node xlvl a x b) y c)
  | ylvl == xlvl = Node xlvl a x (Node ylvl b y c)
skew tree = tree

-- If value is less than nodeValue, insert to left, and right for bigger
-- After insertion, first skew then split at each binary node up to the root
-- Question to myself, do I recurse all the way up to root, or only to to the binary node "nodeValue"?
insert :: Ord a => a -> AATree a -> AATree a
insert value tree = case get value tree of --case distinction
  Just _  -> tree                          -- if value already exist in tree, return tree
  Nothing -> insertLeaf value tree         -- if it doesn't, insert it
    where
      insertLeaf val Empty = Node 1 Empty val Empty   -- if tree empty, make node with value
      insertLeaf val (Node lvl left nodeValue right)
        | val < nodeValue = split (skew (Node lvl (insertLeaf val left) nodeValue right))
        | otherwise = split $ skew $ Node lvl left nodeValue (insertLeaf val right)

-- Test tree

testTree :: AATree Int
testTree = Node 2 (Node 1 Empty 10 (Node 1 (Node 1 Empty 4 Empty) 7 (Node 1 Empty 8 Empty))) 20 (Node 1 Empty 30 Empty)
--Node 2 (Node 1 Empty 10 (Node 1 6 7 8)) 20 (Node 1 Empty 30 Empty)

testTree' :: Num a => AATree a
testTree' = Node 1 Empty 6 Empty

inorder :: AATree a -> [a]
inorder = error "inorder not implemented"

size :: AATree a -> Int
size = error "size not implemented"

height :: AATree a -> Int
height = error "height not implemented"

-- Borrowed code, only for better visualization
toDataTree :: Show a => AATree a -> T.Tree String
toDataTree Empty             = T.Node "⊥" []
toDataTree (Node _ l x r)       = T.Node (show x) $ map toDataTree [r, l]

printTree :: Show a => AATree a -> IO ()
printTree = putStrLn . T.drawTree . toDataTree


--------------------------------------------------------------------------------
-- Optional funct0ion

remove :: Ord a => a -> AATree a -> AATree a
remove = error "remove not implemented"

--------------------------------------------------------------------------------
-- Check that an AA tree is ordered and obeys the AA invariants

checkTree :: Ord a => AATree a -> Bool
checkTree root =
  isSorted (inorder root) &&
  all checkLevels (nodes root)
  where
    nodes x
      | isEmpty x = []
      | otherwise = x:nodes (leftSub x) ++ nodes (rightSub x)

-- True if the given list is ordered
isSorted :: Ord a => [a] -> Bool
isSorted = error "isSorted not implemented"

-- Check if the invariant is true for a single AA node
-- You may want to write this as a conjunction e.g.
--   checkLevels node =
--     leftChildOK node &&
--     rightChildOK node &&
--     rightGrandchildOK node
-- where each conjunct checks one aspect of the invariant
checkLevels :: AATree a -> Bool
checkLevels = error "checkLevels not implemented"

isEmpty :: AATree a -> Bool
isEmpty = error "isEmpty not implemented"

leftSub :: AATree a -> AATree a
leftSub = error "leftSub not implemented"

rightSub :: AATree a -> AATree a
rightSub = error "rightSub not implemented"

--------------------------------------------------------------------------------
