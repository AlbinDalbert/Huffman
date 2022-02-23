module HuffmanTests () where
import Huffman (statistics, maketree, encode, decode) 
import Debug.Trace ( trace )


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

testStatistics :: Bool
testStatistics = statistics "Huffman" == [(1,'H'),(1,'a'),(2,'f'),(1,'m'),(1,'n'),(1,'u')]

testMakeTree :: Bool
--stestMakeTree = maketree [(1,'H'),(1,'a'),(2,'f'),(1,'m'),(1,'n'),(1,'u')] == 

testEncode :: Bool
-- testEncode = sortOutput (phus day2) == sortOutput res_day2

testDecode :: Bool
-- testDecode = sortOutput (phus day3) == sortOutput res_day3
