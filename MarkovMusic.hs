module MarkovMusic
( markovMusic
) where

import Codec.Midi
import Data.String



{- THIS IS NOT AT ALL COMPLETE IM JUST USING IT TO TEST
   -}
markovMusic :: FilePath -> IO () 
markovMusic filepath = do
  x <- (importFile filepath)
  case x of 
    Left x -> return ()
    Right x -> exportFile "out1.mid" (makeMarkovMIDI x)



{- PARSE FUNCTIONS
-}



{- parseFileType parses the fileType of the given Midi file
   -}
parseFileType :: Midi -> FileType
parseFileType midi = fileType midi



{- parseTimeDiv returns the TimeDiv of the given Midi file
   -}
parseTimeDiv :: Midi -> TimeDiv
parseTimeDiv midi = timeDiv midi



{- parseListOfKey returns a listOf the keys which occur in the file
   -}
parseListOfKey :: Midi -> [Key]
parseListOfKey midi = map (\(y,x) -> key x) (filterNoteOnOff midi)



{- parseListOfVelocity returns a listOf the velocities which occur in the file
   -}
parseListOfVelocity :: Midi -> [Velocity]
parseListOfVelocity midi = map (\(y,x) -> velocity  x) (filterNoteOnOff midi)



{- parseListOfTick returns a listOf the ticks which occur in the midi file
   -}
parseListOfTicks :: Midi -> [Ticks]
parseListOfTicks midi = map (\(y,x) -> y) (filterNoteOnOff midi)



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



{- GENERATE FUNCTIONS
-}



-- TODO add the markov chain functions from Armandas' part of the project
{- makeMarkovMIDI produces the markov chain generated Midi file
   -}
makeMarkovMIDI midi = Midi 
  { fileType = parseFileType midi
  , timeDiv = parseTimeDiv midi
  , tracks = [setup midi]++[[(0,TrackEnd)]]
  }



{- setup midi calls settup tracks for our specific midi file
   -}
setup midi = setupTracks (parseListOfTicks midi)
                         (parseListOfVelocity midi)
                         (parseListOfKey midi )



{- Setup tracks given [Ticks], [Velocity], [Key], which are all the same size
   -}
setupTracks ticks velocities keys = foldl (\res (t,v,k) -> res++[(0, NoteOn { channel = 0
                                                                          , key = k
                                                                          , velocity = v})]++
                                                                [(t, NoteOff { channel = 0
                                                                           , key = k
                                                                           , velocity = v})])
                                          [] (zip3 ticks velocities keys) 
