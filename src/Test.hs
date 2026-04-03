type Notation = String
data Accidental = NoAccidental | Flat | Sharp deriving (Show)
data NoteValue = NoteValue 
    { noteNumber :: Int         -- convertion of notenames into numbers from 1 (c) to 12 (b) where 13 would again be c
    , accidental :: Accidental  -- convertion of notenames into numbers from 1 (c) to 12 (b) where 13 would again be c
    , octave     :: Int         -- scientific octave, middle C is in the fourth octave (default: 4)
    , noteLength :: Int         -- corresponds to musescore input like 4 for a quarter note (default: 4)
    , isDotted   :: Bool        -- whether the note is dotted meaning its length is multiplied by 1.5
    } deriving (Show)

-- entering an empty String results in the note: { noteNumber = -1, accidental = NoAccidental, octave = 4, noteLength = 4, isDotted = False }
notation2noteVal :: Notation -> NoteValue
notation2noteVal note = NoteValue  
    { noteNumber = locateNoteName (takeWhile (\c -> c `notElem` "0123456789") note) noteNames 
    , octave     = if noteOctaveAndLength        == "" then 4 else read [head noteOctaveAndLength] -- default is the octave of c'' aka c4
    , accidental = acci
    , noteLength = if drop 1 noteOctaveAndLength == "" then 4 else read [noteOctaveAndLength !! 1] -- default is the quarter note
    , isDotted   = if note == [] then False else (head $ reverse note) == '.' }                    -- default: False
    where noteOctaveAndLength = dropWhile (\c -> c `notElem` "0123456789") note -- also includes the dot if the not is dotted
          locateNoteName name (n : ns) = if name `elem` n then 12 - length ns else locateNoteName name ns
          locateNoteName name [      ] = (-1) -- returning -1 if the name does not match like "h" 
          noteNames = [["c"], ["c#", "db"], ["d"], ["d#", "eb"], ["e"], ["f"], ["f#", "gb"], ["g"], ["g#", "ab"], ["a"], ["a#", "bb"], ["b"]]
          acci = if length note <= 1 then NoAccidental else case head $ drop 1 note of -- default: NoAccidental
              'b' -> Flat
              '#' -> Sharp
              _   -> NoAccidental

main :: IO ()
main = do
    let n  = "f#38"
    let n' = notation2noteVal n
    putStrLn $ "The note " ++ n ++ " is equivalent to the number " ++ show n' ++ "!"
