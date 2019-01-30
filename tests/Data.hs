module Data where

import Data.Tree
import Data.Word
import Numeric.Natural (Natural)
import Data.List.NonEmpty (NonEmpty (..))

mapdata :: [(Int, Rational)]
mapdata = [(10, 20), (3,13), (2242,234), (324, 543.3)]

setdata :: [Int]
setdata = [1,2,3,4,1,2,6,1,4367,832,23,56]

treedata :: Tree Double
treedata = Node 1 [Node 2 [], Node 5 [], Node 6 [Node 7 [], Node 8.9 []]]

textdata :: String
textdata = "Some text! Hello world!"

bytedata :: [Word8]
bytedata = map fromIntegral setdata

nonEmptyNatural :: NonEmpty Natural
nonEmptyNatural = 0 :| [1, 2, 3]

natural1 :: Natural
natural1 = 1
