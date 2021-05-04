module BioFasta.Helpers

import Data.List
import Data.Maybe
import Data.Strings
import BioCore.Sequence
import BioFasta.Common


public export
||| Convert SeqData to String.
toStr : SeqData -> String
toStr (UnSD seqdata) = seqdata

public export
||| Split lines into blocks starting with '>' characters.
||| Filter out # comments (but not semicolons?)
blocks : List (List Char) -> List (List (List Char))
blocks []    = []
blocks blist = map (groupBy (const ('>' /=)))
                   (map (filter ((/= '#')))
                   (map (dropWhile (\x => x /= '>'))
                   (filter (\x => not $ (isNil x)) blist)))
