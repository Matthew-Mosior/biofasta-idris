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
extractSeqLabel : Sequence -> SeqLabel
extractSeqLabel (Seq seqlabel _ _) = seqlabel

public export
extractSeqData : Sequence -> SeqData
extractSeqData (Seq _ seqdata _) = seqdata

public export
extractQualData : Sequence -> Maybe QualData
extractQualData (Seq _ _ qualdata) = qualdata


--Read FASTA file.--

export
||| Convert a FASTA-formatted line into a sequence.
mkSeq : List (List Char) -> Sequence
mkSeq []      = Seq (UnSL "") (UnSD "") Nothing
mkSeq (s::ss) = Seq (UnSL (pack (drop 1 s)))
                    (UnSD (pack (filter (not . isSpace) $ concat $ takeWhile isSeq ss)))
                    Nothing
                      where
                        isSeq : List Char -> Bool
                        isSeq s = if (not . isNil) s &&
                                     (all (isAlpha) s)
                                    then True
                                    else False

export
||| Convert a list of FASTA-formatted lines into a list of sequences.
||| Combines functionality of blocks and mkSeq.
mkSeqs : List (List Char) -> List Sequence
mkSeqs [] = []
mkSeqs s  = map mkSeq (map (lines') s)

public export
||| Read sequences from a FASTA-formatted file.
readFasta : String -> IO (List Sequence)
readFasta f = do Right fasta <- readFile f | Left err => do putStrLn "Could not read in FASTA file: " *> printLn err
                                                            pure []
                 pure (mkSeqs (smallBlocks (unpack fasta)))

--------------------


--Write FASTA file.--

export 
wHead : File -> SeqLabel -> IO ()
wHead f l = do Right addheadersign <- fPutStr f ">" | Left err => putStrLn "Could not add seqlabel header sign." *> printLn err
               Right addseqlabel <- fPutStr f (extractUnSL l) | Left err => putStrLn "Could not add sequence label." *> printLn err
               Right addnewline <- fPutStr f "\n" | Left err => putStrLn "Could not add newline following sequence label." *> printLn err
               pure ()

export
wFasta : File -> Sequence -> IO ()
wFasta f (Seq l d _) = do wHead f l
                          let ls          = splitsAt 59 (unpack (extractUnSD d)) 
                                                        (unpack (extractUnSD d))
                          let mappedls    = intersperse ['\n'] ls
                          let mappedlsstr = pack (concat mappedls)
                          Right addseqdata <- fPutStr f mappedlsstr | Left err => putStrLn "Could not add sequence data following the sequence label." *> printLn err
                          Right addnewline <- fPutStr f "\n" | Left err => putStrLn "Could not add newline following the sequence data." *> printLn err
                          pure ()

export
prepareWriteFasta : File -> List Sequence -> IO ()
prepareWriteFasta f = traverse_ (wFasta f)

public export
||| Write sequences to a FASTA-formatted file.
||| Line length is 60.
writeFasta : String -> List Sequence -> IO ()
writeFasta f ss = do Right hfastaout <- openFile f WriteTruncate | Left err => putStrLn "Could not open FASTA output file: " *> printLn err
                     prepareWriteFasta hfastaout ss
                     closeFile hfastaout

---------------------
