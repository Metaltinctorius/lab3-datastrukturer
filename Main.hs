{-
  to run:
  $ ghc -e main Main.hs < swahili-small.txt

  to compile and run:
  $ ghc -O Main.hs && ./Main < swahili-small.txt
-}

import AATree

--------------------------------------------------------------------------------

main :: IO ()
main = do
  contents <- getContents

  let list = words contents

  -- split the data into words and build an AA tree
  -- use foldl
  let tree = foldl (flip insert) emptyTree list
  --let tree = createTree emptyTree list

  let s = size tree
  let h = bstHeight tree
  let c = checkTree tree
  let oh = ceiling (logBase 2 (fromIntegral (s + 1))) - 1
  let r = fromIntegral h / fromIntegral oh
  let fw = take 20 (inorder tree)


  -- calculate and print statistics
  -- use fromIntegral/ceiling/logBase
  putStrLn $ unlines
              [ "Size: " ++ show s
              , "Height: " ++ show h
              , "Optimal height: " ++ show oh
              , "Height / Optimal height: " ++ show r
              , "checkTree: " ++ show c
              , "First 20 words: " ++ show fw
              ]

-- Creating a tree using standard recursion
-- Created for practice, to better understand what is happening
-- under the hood, generating the same tree as with foldl
createTree :: Ord a => AATree a -> [a] -> AATree a
createTree tree [] = tree
createTree tree [x] = insert x tree
createTree tree (x:xs) = createTree (insert x tree) xs

--------------------------------------------------------------------------------

