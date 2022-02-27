module HuffmanTests () where
import Huffman 
import Debug.Trace ( trace )
import Data.Tree

{-
allTests :: Bool
allTests =  trace "-----------"
            trace ("test Statistics: " ++ show testStatistics)
            trace ("test MakeTree: " ++ show testMakeTree)
            trace ("test Encode: " ++ show testEncode)
            trace ("test Decode: " ++ show testDecode)
            trace "-----------"
            testDay0 &&
            testDay1 &&
            testDay2 &&
            testDay3 &&
            testDay4 &&
            testDay5 &&
            testDay6
-}
testStatistics :: Bool
testStatistics = statistics "Huffman" == [(1,'H'),(1,'a'),(2,'f'),(1,'m'),(1,'n'),(1,'u')]

-- Branch {h0 = Branch {h0 = Leaf {c = 'H'}, h1 = Leaf {c = 'f'}}, h1 = Branch {h0 = Branch {h0 = Leaf {c = 'u'}, h1 = Leaf {c = 'n'}}, h1 = Branch {h0 = Leaf {c = 'm'}, h1 = Leaf {c = 'a'}}}}


testWeightMakeTree :: Bool
testWeightMakeTree = getTreeW (makeWtree (generateWtreeList (statistics "Huffman") [])) == 7
-- stestMakeTree = maketree [(1,'H'),(1,'a'),(2,'f'),(1,'m'),(1,'n'),(1,'u')] == 

-- testEncode :: Bool
-- testEncode = sortOutput (phus day2) == sortOutput res_day2

-- testDecode :: Bool
-- testDecode = sortOutput (phus day3) == sortOutput res_day3
