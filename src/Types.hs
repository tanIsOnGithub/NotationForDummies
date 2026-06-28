module Types (
    Notation,
    Accidental (..),
    NoteValue (..),
    World (..),
) where


type Notation = String

-- the accidental of a note which is either flat, sharp or non-existant (NoAccidental)
data Accidental = NoAccidental | Flat | Sharp deriving (Show, Eq)

-- structure for the content of a note (without graphical elements)
data NoteValue = NoteValue 
    { noteNumber :: Int         -- conversion of notenames into numbers from 1 (c) to 12 (b) where 13 would again be c
    , accidental :: Accidental  -- see the corresponding datatype (Accidental) 
    , octave     :: Int         -- scientific octave, middle C is in the fourth octave (default: 4)
    , noteLength :: Int         -- corresponds to musescore input like 4 for a quarter note (default: 4)
    , isDotted   :: Bool        -- whether the note is dotted meaning its length is multiplied by 1.5
    } deriving (Show)

-- structure contains everything 
data World = World 
    -- list of all notes as a string
    [NoteValue]  
    -- index of the currently selecte note
    -- (-1) means no selection
    Int         
