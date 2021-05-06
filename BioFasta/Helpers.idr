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
smallBlocks : List Char -> List (List Char)
smallBlocks blist = groupBy (const (/= '>'))
                    (filter (\x => x /= '#')
                    (dropWhile (\x => x /= '>')
                    (filter (\x => x /= ' ') blist)))
