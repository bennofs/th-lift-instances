module Data where

import Data.Tree
import Data.Word

mapdata :: [(Int, Int)]
mapdata = [(10, 20), (3,13), (2242,234), (324, 543)]

setdata :: [Int]
setdata = [1,2,3,4,1,2,6,1,4367,832,23,56]

treedata :: Tree Int
treedata = Node 1 [Node 2 [], Node 5 [], Node 6 [Node 7 [], Node 8 []]]

textdata :: String
textdata = "Some text! Hello world!"

bytedata :: [Word8]
bytedata = map fromIntegral setdata
