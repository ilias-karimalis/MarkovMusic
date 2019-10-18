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
setup midi = setupTracks [ round x | x <- (makenewlist (parseListOfTicks midi) 25 [])]
                         [ round x | x <- (makenewlist (parseListOfVelocity midi) 25 [])]
                         [ round x | x <- (makenewlist (parseListOfKey midi) 25 [])]



{- Setup tracks given [Ticks], [Velocity], [Key], which are all the same size
   -}
setupTracks ticks velocities keys = foldl (\res (t,v,k) -> res++[(0, NoteOn { channel = 0
                                                                          , key = k
                                                                          , velocity = v})]++
                                                                [(t, NoteOff { channel = 0
                                                                           , key = k
                                                                           , velocity = v})])
                                          [] (zip3 ticks velocities keys) 



{- MARKOV CHAIN
-}

--main prediction function encapsulates markov chain functions--




--provides a square table of zeroes whose size is the length of the uniques of a list
--columns and rows are labelled by unique items in list
--matrix:: Num int => [int] -> [(int, int, int)]
matrix [] = []
matrix list = [(key1, key2, 0) | key1 <- (uniques list), key2 <- (uniques list)]

makenewlist list n newlist
    |n == 0 = newlist
    |newlist == [] = makenewlist list (n - 1) [(pickfirst list)]
    |otherwise = makenewlist list (n -1) ((picknext (head newlist) (options (head newlist) (proptoprob (populate list (matrix (list)))))) : newlist)

picknext item (head:opts)
    |opts == [] = second head
    |(third head) >= random = second head
    | otherwise = picknext item (proptoprob (opts))

addtothird n (a,b,c) = (a, b, (c + n))
third (a,b,c) = c
second (a,b,c) = b

sumcolumn d [] = []
sumcolumn d ((a,b,c):t)
    | d == a = c + sumcolumn d t
    | otherwise = sumcolumn d t

--proptoprob ((a,b,c):t)
--    | t == [] = (a,b,c)
--    |otherwise = ((a,b, (c/ (sumcolumn a ((a,b,c):t)))): proptoprob t)

proptoprob ((a,b,c):t)
    | t == [] = [(a,b,c)]
    |otherwise = (a,b, (c/ (sumcolumn a ((a,b,c):t)))):(proptoprob t)

options item = filter (match1 item)

match1 item (key1, key2, prop) = item == key1

--pickfirst list = list!!(round ((length list) * random))

pickfirst list = list!!(round ((fromIntegral (length list)) * random))

random = 0.1


--uses matrix to create a matrix of zeroes and then replaces the zeroes with probabilites
--populate:: Eq int Num prop=> [int] -> [(int, int, prop)] -> [(int, int, prop)]
--goes through a list of keys, velocities, or ticks
--filters unique items in that list
--builds triplets such that each unique pair has a triplet, originally created with a zero
--goes through 

populate :: (Num a, Eq a) => [a] -> [(a,a,a)] -> [(a,a,a)]
populate [] lst = lst
populate (h:[]) lst = map (add1key1 h) lst
populate (h:h2:t) lst = populate (h2:t) (map (add1key1key2 h h2) lst)

--add1key1:: Eq a => a -> (a, a, a) -> (a, a, a)
add1key1 h (key1, key2, prop)
    | key1 == h = (key1, key2, (prop + 1))
    | otherwise = (key1, key2, prop)


add1key1key2 h n (key1, key2, prop)
    | (key1 == h && key2 == n) = (key1, key2, (prop + 1))
    | otherwise = (key1, key2, prop)



--returns only the unique items in a list
uniques:: Eq a => [a] -> [a]
uniques [] = []
uniques (h:t)
    | elem h t = uniques t
    | otherwise = h:(uniques t)