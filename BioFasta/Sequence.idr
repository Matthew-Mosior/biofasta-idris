module BioFasta.Sequence

import Data.List
import Data.Maybe
import Data.Strings
import BioCore.Sequence
import BioFasta.Common
import BioFasta.Helpers
import System.File


public export
data Sequence = ||| A data type representing a fasta sequence.
                Seq SeqLabel SeqData (Maybe QualData)
               
public export
Eq Sequence where
  (==) (Seq l d mqd) (Seq l' d' mqd') = l == l' && d == d' && mqd == mqd'
  (==) _             _                = False

public export
Show Sequence where
  show (Seq l d mpd) = (show l) ++ 
                       (show d) ++ 
                       (show mpd)


public export
||| Convert a FASTA-formatted line into a sequence.
mkSeq : List (List Char) -> Sequence
mkSeq []      = Seq (UnSL []) (UnSD "") Nothing
mkSeq (s::ss) = Seq (UnSL (drop 1 s))
                    (UnSD (pack (filter (not . isSpace) $ concat $ takeWhile isSeq ss)))
                    Nothing
                      where
                        isSeq : List Char -> Bool
                        isSeq s = if (not . isNil) s &&
                                     (all (isAlpha) s)
                                    then True
                                    else False           

public export
||| Convert a list of FASTA-formatted lines into a list of sequences.
||| Combines functionality of blocks and mkSeq.
mkSeqs : List (List Char) -> List Sequence
mkSeqs [] = []
mkSeqs s  = map mkSeq (blocks s)


public export
||| Read sequences from a FASTA-formatted file.
readFasta : String -> IO (List Sequence)
readFasta f = do Right fastaaslist <- readFile f | Left err => do putStrLn "Could not read in FASTA file: " *> printLn err
                                                                  pure []
                 pure (mkSeqs (lines' (unpack fastaaslist)))

public export
||| Write sequences to a FASTA-formatted file.
||| Line length is 60.
writeFasta : String -> List Sequence -> IO ()
writeFasta f ss = do Right fastaaslist <- writeFile f (show ss) | Left err => putStrLn "Could not write FASTA file: " *> printLn err
                     pure ()
