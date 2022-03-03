{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Huffman (statistics, maketree, encode, decode, Htree) where
import Data.List (find, delete, sortBy, sortOn)
import Data.Maybe (isJust, fromJust)
import GHC.Char ( chr )
import Data.Tree ()

data Htree = Leaf {c :: Char} | Branch {h0 :: Htree, h1 :: Htree} deriving (Show, Read, Eq)
data Wtree = L {weight :: Integer, cha :: Char} | B {weight :: Integer, t1 :: Wtree, t2 :: Wtree}

type BitTableElem = (Char, [Integer])


-- Encode ------------------------------------------------------------------------------------------
-- Input:   The string to be compressed
-- Output:  The resulting bit sequence and huffman tree which is used in decompression

encode :: String -> (Htree , [Integer])
encode str =    let tree = maketree (statistics str)
                in (tree, makeBitSequence tree (makeBitTable tree []) str [])


-- Makes a bit table with each chars bit sequence.
makeBitTable :: Htree -> [Integer] -> [BitTableElem]
makeBitTable (Leaf c) bits = [(c, bits)]
makeBitTable (Branch t1 t2) bits = makeBitTable t1 (bits ++ [0]) ++ makeBitTable t2 (bits ++ [1])


-- generates the bit sequence from the huffman tree and bitTable
makeBitSequence :: Htree -> [BitTableElem]-> String -> [Integer] -> [Integer]
makeBitSequence _ _ [] bitSeq = bitSeq
makeBitSequence tree bitTable (c:rest) bitSeq = makeBitSequence tree bitTable rest (bitSeq ++ getBitsForChar c bitTable)


-- returns the bit sequence for the given char
getBitsForChar :: Char -> [BitTableElem] -> [Integer]
getBitsForChar c ((ch, bits):rest) =    if c == ch
                                        then bits
                                        else getBitsForChar c rest


-- Statistics -----------------------------------------------------------------------------
-- Input:   A String to generate frequency table of
-- Output:  A list of tuples with a Char from the string and 
--          it's corresponding frequency in the string

statistics :: String -> [(Integer, Char)]
statistics text = filtering (statRec text (replicate 256 0))


-- The recursive function of statistics above
statRec :: String -> [Integer] -> [(Integer, Char)]
statRec [] list = zip list (map chr [0..255])
statRec (c:rest) list = statRec rest (replaceAtIndex (fromEnum c) ((list!!fromEnum c)+1) list)


-- filter away all chars with 0 in frequency
filtering :: [(Integer, Char)] -> [(Integer, Char)]
filtering = filter (\(x,_) -> x /= 0)


-- A custom replacement function
replaceAtIndex :: Int -> Integer -> [Integer] -> [Integer]
replaceAtIndex index numb list =    let (x,_:y) = splitAt index list
                                    in x++numb:y


-- MakeTree --------------------------------------------------------------------------------------------
-- Input:   A frequency table generated by the function 'statistics'
-- Output:  A huffman tree with the most frequent chars high up and less frequent further down

maketree :: [(Integer, Char)] -> Htree
maketree [leaf] = wtreeToHtree (makeWtree (generateWtreeList ([leaf]++[(0,'\0')]) []))
maketree list = wtreeToHtree ( makeWtree (generateWtreeList list []))


-- makes a weighted tree, used to make a effective huffman tree
makeWtree :: [Wtree] -> Wtree
makeWtree [root] = root
makeWtree list =    if length list > 1
                    then makeWtree (sortList (drop 2 list ++ [createWtreeOfTrees (head list) (last (take 2 list))]))
                    else head list


-- Sort list of weighted leafs and subtrees
sortList :: [Wtree] -> [Wtree]
sortList = sortOn weight


-- make a new tree with the two given trees as children
createWtreeOfTrees :: Wtree -> Wtree -> Wtree
createWtreeOfTrees t1 t2 = B (getTreeW t1 + getTreeW t2) t1 t2


-- returns the weight of a given tree
getTreeW :: Wtree -> Integer
getTreeW (B weight _ _) =  weight
getTreeW (L weight _) =  weight


-- returns the Char of a given leaf
getTreeC :: Wtree -> Char
getTreeC (L _ cha) =  cha


-- generates a list of leafs from a frequency table
generateWtreeList :: [(Integer, Char)] -> [Wtree] -> [Wtree]
generateWtreeList [] wTreeList = wTreeList
generateWtreeList ((i,c):rest) wTreeList = generateWtreeList rest (L i c : wTreeList)


-- transform a weighted tree into a huffman tree
wtreeToHtree :: Wtree -> Htree
wtreeToHtree (L _ c) = Leaf c
wtreeToHtree (B _ t1 t2) = Branch (wtreeToHtree t1) (wtreeToHtree t2)


-- Decode -------------------------------------------------------------------------------------
-- Input:   A huffman tree used to traverse the bit sequence, and the sequence to decompress
-- Output:  The resulting string from the compression

decode :: Htree -> [Integer] -> String
decode tree bits = decodeTraverse tree tree bits []


-- traverse the string, and the tree for each char in the string
decodeTraverse :: Htree -> Htree -> [Integer] -> String -> String
decodeTraverse _ (Leaf c) [] str = str++[c]
decodeTraverse root (Leaf c) bits newStr = decodeTraverse root root bits (newStr ++ [c])
decodeTraverse root (Branch t0 t1) (b:bits) newStr =  if b == 0
                                                    then decodeTraverse root t0 bits newStr
                                                    else decodeTraverse root t1 bits newStr



-- Tests -------------------------------------------------------------------------------------

testStatistics :: Bool
testStatistics = statistics "Huffman" == [(1,'H'),(1,'a'),(2,'f'),(1,'m'),(1,'n'),(1,'u')]


testWeightMakeTree :: Bool
testWeightMakeTree = getTreeW (makeWtree (generateWtreeList (statistics "Huffman") [])) == 7


testMakeTree :: Bool
testMakeTree = show (maketree (statistics "Huffman")) == "Branch {h0 = Branch {h0 = Leaf {c = 'H'}, h1 = Leaf {c = 'f'}}, h1 = Branch {h0 = Branch {h0 = Leaf {c = 'u'}, h1 = Leaf {c = 'n'}}, h1 = Branch {h0 = Leaf {c = 'm'}, h1 = Leaf {c = 'a'}}}}"


testEncode :: Bool 
testEncode =    let (_, bitList) = encode "Huffman"
                in bitList == [0,0,1,0,0,0,1,0,1,1,1,0,1,1,1,1,0,1]

-- their is no test for only decode as making one would still need to do all the work
-- encode does. And because encode has a unique test, one could
-- deduct that decode is wrong if testEncode returns true and this test returns false.
testEncodeAndDecode :: Bool
testEncodeAndDecode = uncurry decode (encode "Huffman") == "Huffman"