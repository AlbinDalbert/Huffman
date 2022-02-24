module Huffman {-(statistics, maketree, encode, decode, Htree)-} where
import Data.Ord (comparing)
import Data.List (find, delete, sortBy, sortOn)
import Data.Maybe (isJust, fromJust)
import Distribution.Simple.Program.HcPkg (list)
import Data.Char(intToDigit)

import GHC.Char ( chr )
import Data.Function ()
import Data.Vector (create)
import Distribution.Compat.Lens (getting)
import Control.Monad (when)
import Data.Tree 
import Text.Show

data Htree = Leaf {c :: Char} | Branch {h0 :: Htree, h1 :: Htree} deriving (Show, Read, Eq)


data Wtree = L {weight :: Integer, cha :: Char} | B {weight :: Integer, t1 :: Wtree, t2 :: Wtree}

-- COMPLETED --
-- Statistics -----------------------------------------------------------------------------
statistics :: String -> [(Integer, Char)]
statistics text = filtering (statRec text (replicate 256 0))


statRec :: String -> [Integer] -> [(Integer, Char)]
statRec [] list = zip list (map chr [0..255])
statRec (c:rest) list = statRec rest (replaceAtIndex (fromEnum c) ((list!!fromEnum c)+1) list)


filtering :: [(Integer, Char)] -> [(Integer, Char)]
filtering = filter (\(x,_) -> x /= 0)


replaceAtIndex :: Int -> Integer -> [Integer] -> [Integer]
replaceAtIndex index numb list =    let (x,_:y) = splitAt index list
                                    in x++numb:y


letterExistInList :: Char -> [(Integer, Char)] -> Bool
letterExistInList cha list = isJust (find (\(_,x) -> x == cha) list)


findTuple :: Char -> [(Integer, Char)] -> Maybe (Integer, Char)
findTuple c = find (\(_,x) -> x == c)


-- MakeTree --------------------------------------------------------------------------------------------
maketree :: [(Integer, Char)] -> Htree
maketree list = wtreeToHtree ( makeWtree (generateWtreeList list []))

-- CONFIRMED --
--sortList :: [(Integer, Char)] -> [(Integer, Char)]
--sortList = sortBy (comparing fst)

zipLeafAndWeight :: [Wtree] -> [Integer] -> [Char] -> [(Integer, Char)]
zipLeafAndWeight [] inte cha = zip inte cha
zipLeafAndWeight (x:xs) inte cha = zipLeafAndWeight xs (getTreeW x : inte) (getTreeC x : cha)


-- CONFIRMED
makeWtree :: [Wtree] -> Wtree
makeWtree [root] = root
makeWtree list =    if length list > 1
                    then makeWtree (sortList (drop 2 list ++ [createWtreeOfTrees (head list) (last (take 2 list))]))
                    else head list


sortList :: [Wtree] -> [Wtree]
sortList = sortOn weight


createWtreeOfTrees :: Wtree -> Wtree -> Wtree
createWtreeOfTrees t1 t2 = B (getTreeW t1 + getTreeW t2) t1 t2


getTreeW :: Wtree -> Integer
getTreeW (B weight _ _) =  weight
getTreeW (L weight _) =  weight

getTreeC :: Wtree -> Char
-- getTreeC (B weight _ _) =  weight
getTreeC (L _ cha) =  cha


generateWtreeList :: [(Integer, Char)] -> [Wtree] -> [Wtree]
generateWtreeList [] wTreeList = wTreeList
generateWtreeList ((i,c):rest) wTreeList = generateWtreeList rest (L i c : wTreeList)


wtreeToHtree :: Wtree -> Htree 
wtreeToHtree (L _ c) = Leaf c
wtreeToHtree (B _ t1 t2) = Branch (wtreeToHtree t1) (wtreeToHtree t2)

-- //TODO Encode
-- Encode ------------------------------------------------------------------------------------------
encode :: String -> (Htree , [Integer])
encode _ = error "Not Implemented Yet"


-- //TODO Decode
-- Decode -------------------------------------------------------------------------------------
decode :: Htree -> [Integer] -> String
decode _ _ = error "Not Implemented Yet"