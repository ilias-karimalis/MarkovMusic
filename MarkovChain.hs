module MarkovChain
( getFile,
  importThenBuild,
  getListOfVelocity,
  getListOfTicks
) where

import Codec.Midi
import Data.String

getFile :: FilePath -> IO (Either String Midi)
getFile filepath = importFile filepath



{- THIS IS NOT AT ALL COMPLETE IM JUST USING IT TO TEST
   -}
importThenBuild :: FilePath -> IO [Char] 
importThenBuild filepath = do
  x <- (getFile filepath)
  case x of 
    Left x -> return "fail"
    Right x -> return (show (getListOfTicks x)) 

{- getListOfKey returns a listOf the keys which occur in the file
   -}
getListOfKey :: Midi -> [Key]
getListOfKey midi = map (\(y,x) -> key x) (filterNoteOnOff midi)



{- getListOfVelocity returns a listOf the velocities which occur in the file
   -}
getListOfVelocity :: Midi -> [Velocity]
getListOfVelocity midi = map (\(y,x) -> velocity  x) (filterNoteOnOff midi)



{- getListOfTick returns a listOf the ticks which occur in the midi file
   -}
getListOfTicks :: Midi -> [Ticks]
getListOfTicks midi = map (\(y,x) -> y) (filterNoteOnOff midi)



{- filterNoteOnOff returns a listOf tracks wherein every track has
  - a message of type NoteOn or NoteOff
  -}
filterNoteOnOff :: Midi -> [(Ticks, Message)]
filterNoteOnOff midi = filter (\(y,x) -> ((isNoteOn x) ||
                                         (isNoteOff x)))
                                         (mergeLists (tracks midi))



{- mergeLists merges a ListOf ListOf a into a ListOf a
   -}
mergeLists :: Foldable t => t [a] -> [a]
mergeLists lst = foldl (\x res -> x ++ res) [] lst
