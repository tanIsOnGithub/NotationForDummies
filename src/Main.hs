module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- data types --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- basic structure for a Note stored using either a specific string or the type specifically created for it
-- the notes in this code are named using the scientific pitch notation with lower-case letters, e.g. "c''" corresponds to "c4"
-- with added numbers for their length which correspond to the number typed in musescore as in "c#34." denotes 
-- a dotted c-sharp quater note in the third octave
type Notation = String

-- the accidental of a note which is either flat, sharp or non-existant (NoAccidental)
data Accidental = NoAccidental | Flat | Sharp deriving (Show)

data NoteValue = NoteValue 
    { noteNumber :: Int         -- conversion of notenames into numbers from 1 (c) to 12 (b) where 13 would again be c
    , accidental :: Accidental  -- conversion of notenames into numbers from 1 (c) to 12 (b) where 13 would again be c
    , octave     :: Int         -- scientific octave, middle C is in the fourth octave (default: 4)
    , noteLength :: Int         -- corresponds to musescore input like 4 for a quarter note (default: 4)
    , isDotted   :: Bool        -- whether the note is dotted meaning its length is multiplied by 1.5
    } deriving (Show)

-- structure contains everything 
data World = World [Notation]

-- spacing constants -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- window
wSizeX     :: Float; wSizeY     :: Float; (wSizeX    , wSizeY    ) = (1600.0, 900.0) -- window sizes in both dimensions as FLOATS
wCenteredX :: Float; wCenteredY :: Float; (wCenteredX, wCenteredY) = ((-wSizeX) / 2, (-wSizeY) / 2)

-- notation
gapBetweenStaves  :: Float; gapBetweenStaves  = wSizeY / 60
topperToSystemGap :: Float; topperToSystemGap = wSizeY * 4 / 15      -- difference between the top of the window and the highest staff line
startOfNotes      :: Float; startOfNotes      = wSizeX / 32          -- x-coordinate of the beginning of notes on a staff line
noteHeadRadius    :: Float; noteHeadRadius    = gapBetweenStaves / 2 -- radius of a notehead which is approximated by a circle
pixelRoundingErrorOffset :: Float; pixelRoundingErrorOffset = 3 -- aligns the noteheads perfectly inside of drawNotation

-- helper functions -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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

-- main functions -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Initial world state
initialWorld :: World
initialWorld = World ["c4"]

-- Convert world state to a picture
drawWorld :: World -> Picture
drawWorld  (World w) = pictures $ staveLines : map drawNotation w
   where staveLines = color white . pictures . map makeLine $ staveLinesPositions
         makeLine ly = line [(wCenteredX, ly), (wCenteredX + wSizeX, ly)] 
         -- drawing the staff lines from top to bottom
         staveLinesPositions = map (\n -> wSizeY / 2 - topperToSystemGap - n * gapBetweenStaves) [0..4]

drawNotation :: Notation -> Picture
drawNotation _ = color white . pictures $ [noteStem, noteHead]
    -- position of the octave above middle C, c4
    where (cx, cy) = ((-wSizeX) / 2 + startOfNotes, wSizeY / 2 - topperToSystemGap - noteHeadRadius / 2 - noteHeadRadius * 2 - pixelRoundingErrorOffset)
          noteStem = line [(cx - noteHeadRadius, cy), (cx - noteHeadRadius, cy - gapBetweenStaves * 3.5)]
          noteHead = translate cx cy . circleSolid $ noteHeadRadius

-- not updating the world via events yet
handleEvent :: Event -> World -> World
handleEvent _ world = world

-- not updating the world by time yet
updateWorld :: Float -> World -> World
updateWorld _ world = world

-- Main function using displayIO
main :: IO ()
main = play
        (InWindow "Notation software for dummies" -- window
        (round wSizeX, round wSizeY) (round wCenteredX, round wCenteredY)) -- centering in the middle 
    black                                         -- Background color
    600                                           -- 60 fps
    initialWorld                                  -- Initial state
    drawWorld                                     -- Drawing function
    handleEvent                                   -- Event handler
    updateWorld                                   -- Update function
