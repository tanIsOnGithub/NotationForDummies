module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace 

-- data types --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- basic structure for a Note stored using either a specific string or the type specifically created for it
-- the notes in this code are named using the scientific pitch notation with lower-case letters, e.g. "c''" corresponds to "c4"
-- with added numbers for their length which correspond to the number typed in musescore as in "c#34." denotes 
-- a dotted c-sharp quater note in the third octave
type Notation = String

-- the accidental of a note which is either flat, sharp or non-existant (NoAccidental)
data Accidental = NoAccidental | Flat | Sharp deriving (Show)

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

-- spacing constants -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- window
wSizeX     :: Float; wSizeY     :: Float; (wSizeX    , wSizeY    ) = (3200.0, 1800.0) -- window sizes in both dimensions as FLOATS
wCenteredX :: Float; wCenteredY :: Float; (wCenteredX, wCenteredY) = ((-wSizeX) / 2, (-wSizeY) / 2)

-- notation
gapBetweenStaves  :: Float; gapBetweenStaves  = wSizeY / 60
topperToSystemGap :: Float; topperToSystemGap = wSizeY * 4 / 15        -- difference between the top of the window and the highest staff line
startOfNotes      :: Float; startOfNotes      = wSizeX / 32            -- x-coordinate of the beginning of notes on a staff line
spaceBetweenNotes :: Float; spaceBetweenNotes = gapBetweenStaves * 2.5 -- horizontal space between two notes
noteHeadRadius    :: Float; noteHeadRadius    = gapBetweenStaves / 2   -- radius of a notehead which is approximated by a circle
pixelRoundingErrorOffset :: Float; pixelRoundingErrorOffset = 7 -- aligns the noteheads perfectly inside of drawNotation
selectionColor :: Color; selectionColor = violet -- color of selected notes

-- helper functions -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- basic helper function for linked list 
editNote :: [NoteValue] -> Int -> NoteValue -> [NoteValue]
editNote notes i n = start ++ [n] ++ end 
    where (start, end) = helper $ splitAt i notes 
          helper (s, (_ : e)) = (s, e)
          helper (s, [     ]) = (s, [])

-- moving a note vertically (the function expects either KeyDown or 
-- KeyUp as input for the parameter "k")
moveNoteV :: [NoteValue] -> Int -> SpecialKey -> [NoteValue]
moveNoteV notes selectionIndex k = editNote notes selectionIndex editedNote
    where curNote = notes !! selectionIndex 
          curNum  = traceWith (\s -> "curNum: " ++ show s) (noteNumber curNote)
          newNum  = if k == KeyDown then curNum - 1 else curNum + 1
          editedNote = NoteValue { noteNumber =            traceWith (\s -> "newNum: " ++ show s) newNum
                                 , accidental = accidental curNote 
                                 , octave     = octave     curNote
                                 , noteLength = noteLength curNote
                                 , isDotted   = isDotted   curNote }

-- entering an empty String results in the note: { noteNumber = -1, accidental = NoAccidental, octave = 4, noteLength = 4, isDotted = False }
noteString2noteVal :: Notation -> NoteValue
noteString2noteVal note = NoteValue  
    { noteNumber = locateNoteName (takeWhile (\c -> c `notElem` "0123456789") note) noteNames 
    , octave     = if noteOctaveAndLength        == "" then 4 else read [head noteOctaveAndLength] -- default is the octave of c'' aka c4
    , accidental = acci
    , noteLength = if drop 1 noteOctaveAndLength == "" then 4 else read [noteOctaveAndLength !! 1] -- default is the quarter note
    , isDotted   = if note == [] then False else (head $ reverse note) == '.' }                    -- default: False
    where noteOctaveAndLength = dropWhile (\c -> c `notElem` "0123456789") note -- also includes the dot if the not is dotted
          locateNoteName name (n : ns) = if name `elem` n then 12 - length ns else locateNoteName name ns
          locateNoteName _    [      ] = (-1) -- returning -1 if the name does not match like "h" 
          noteNames = [["c"], ["c#", "db"], ["d"], ["d#", "eb"], ["e"], ["f"], ["f#", "gb"], ["g"], ["g#", "ab"], ["a"], ["a#", "bb"], ["b"]]
          acci = if length note <= 1 then NoAccidental else case head $ drop 1 note of -- default: NoAccidental
              'b' -> Flat
              '#' -> Sharp
              _   -> NoAccidental

-- main functions -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Initial world state
initialWorld :: World
initialWorld = World (map noteString2noteVal ["c"]) (0)

-- Convert world state to a picture
drawWorld :: World -> Picture
drawWorld (World w selectionIndex) = pictures $ staveLines : drawNotation w 0 selectionIndex
   where staveLines = color white . pictures . map makeLine $ staveLinesPositions
         makeLine ly = line [(wCenteredX, ly), (wCenteredX + wSizeX, ly)] 
         -- drawing the staff lines from top to bottom
         staveLinesPositions = map (\n -> wSizeY / 2 - topperToSystemGap - n * gapBetweenStaves) [0..4]

drawNotation :: [NoteValue] -> Int -> Int -> [Picture]
drawNotation [                   ] _ _ = []
drawNotation (noteVal  : notes) i selectionIndex = 
          (color noteColor . pictures $ [noteStem, noteHead]) : drawNotation notes (i + 1) selectionIndex
          -- position of the octave above middle C: c4
    where cx = (-wSizeX) / 2 + startOfNotes
          cy = wSizeY / 2 - topperToSystemGap - noteHeadRadius / 2 - noteHeadRadius * 2 - pixelRoundingErrorOffset
          -- position of the current note
          x = cx + fromIntegral i * spaceBetweenNotes
          y = cy + (traceWith (\s -> "adjusted note Number: " ++ show s) . adjustNoteNum . traceWith (\s -> "noteNumber: " ++ show s) . noteNumber $ noteVal) * noteHeadRadius
          -- converting the raw noteNumber (character to number) to 
          -- the visual gap from middle c (adjusting for accidentals)
          adjustNoteNum n = case n of
                              1  -> 0
                              2  -> 0
                              3  -> 1
                              4  -> 1
                              5  -> 2
                              6  -> 3
                              7  -> 3
                              8  -> 4
                              9  -> 4
                              10 -> 5
                              11 -> 5
                              12 -> 6
                              _  -> (-1) --should never happen
          -- note head, stem and color
          noteStem  = line [(x - noteHeadRadius, y), (x - noteHeadRadius, y - gapBetweenStaves * 3.5)]
          noteHead  = translate x y . circleSolid $ noteHeadRadius
          noteColor = if i == selectionIndex then selectionColor else white

-- not updating the world via events yet
-- EventKey Key KeyState Modifiers (Float, Float)    
handleEvent :: Event -> World -> World
handleEvent (EventKey (SpecialKey k) Down _ _) world = handleArrowKeys world k
handleEvent (_                  ) world = world

-- if the user goes left from the leftmost note, it is deselected and can be reselected
-- by pressing right again
handleArrowKeys :: World -> SpecialKey -> World
handleArrowKeys (World notes selectionIndex) k = if newIndex == (-2) then World editedNotes (-1) else World editedNotes newIndex
    where newIndex = (selectionIndex + selectionIndexMovement)
          -- changing the current Note 
          editedNotes = if k == KeyUp || k == KeyDown
                        then moveNoteV notes selectionIndex k 
                        else notes

          selectionIndexMovement = case k of
                                     KeyUp    ->   0
                                     KeyDown  ->   0
                                     KeyRight ->   1
                                     KeyLeft  -> (-1)
                                     _        ->   0

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
