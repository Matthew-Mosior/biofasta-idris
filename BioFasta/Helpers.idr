module BioFasta.Helpers

import Data.List
import Data.Maybe
import Data.Strings
import BioCore.Sequence
import BioFasta.Common

%default total 


public export
||| Convert SeqData to String.
toStr : SeqData -> String
toStr (UnSD seqdata) = seqdata

public export
notLast : (l :(List (List Char))) -> {auto ok : NonEmpty l} -> List (List Char)
notLast l = init l

public export
splitsAt : Offset -> List Char -> List Char -> List (List Char)
splitsAt _ _ []       = []
splitsAt n s (_::ss') = let (s1,s2) = splitAt (integerToNat 
                                              (prim__cast_IntInteger 
                                              (extractUnOff n))) s
                          in if isNil s2
                               then [s1]
                               else s1 :: splitsAt n s2 ss'

public export
smallBlocks : List Char -> List (List Char)
smallBlocks blist = groupBy (const (/= '>'))
                    (filter (\x => x /= '#')
                    (dropWhile (\x => x /= '>')
                    (filter (\x => x /= ' ') blist)))
