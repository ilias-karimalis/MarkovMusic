module MarkovChain
( importFile
,
) where

import Codec.Midi
import Data.String

getFile :: FilePath -> IO (Either String Midi)
getFile filepath = importFile filepath

importThenBuild filepath = do
  x <- (getFile filepath)
  case x of 
    Left x -> return "fail"
    Right x -> return (show x) 
  
{- getTicks returns a list of pairs of channel listOf Ticks pairs
  - where the listOf Ticks for a certain channel is in the sequential
  - order in which it appears in the midi file. 
  -}
--getTicks :: Midi -> [(Channel, [Ticks])]
getTicks midi = map (\(t, msg)->((channel msg),[t])) (filterNoteOnOff midi)



{- filterNoteOnOff returns a listOf tracks wherein every track has
  - a message of type NoteOn or NoteOff
  -}
filterNoteOnOff midi = filter (\(y,x) -> ((isNoteOn x) ||
                                         (isNoteOff x)))
                                         (tracks midi)
