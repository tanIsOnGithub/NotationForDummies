-- TODO: debug thickLine via pure coding magic lmao
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace 
import Draw (drawWorld, wSizeX, wSizeY, wCenteredX, wCenteredY)
import Types

-- data types --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- basic structure for a Note stored using either a specific string or the type specifically created for it
-- the notes in this code are named using the scientific pitch notation with lower-case letters, e.g. "c''" corresponds to "c4"
-- with added numbers for their length which correspond to the number typed in musescore as in "c#34." denotes 
-- a dotted c-sharp quater note in the third octave

-- spacing constants ----------------------------------------------------------------------------------------------------------------------------

-- [a, b, c, d] = [(-1485.0,345.5),(-1485.0,347.5),(-1485.0,452.5),(-1485.0,450.5)] 

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
          curNum  = noteNumber curNote
          (newNum, newOctave, newAcci)  = computeNewNote curNum (octave curNote) k
          editedNote = NoteValue { noteNumber =            newNum
                                 , accidental =            newAcci 
                                 , octave     =            newOctave
                                 , noteLength = noteLength curNote
                                 , isDotted   = isDotted   curNote }

-- helper for moveNoteV, returns (newNum, newOctave, newAccidental)
computeNewNote :: Int -> Int -> SpecialKey -> (Int, Int, Accidental)
computeNewNote num oct k
    | num == 1  && k == KeyDown = (12, oct - 1, NoAccidental)
    | num == 12 && k == KeyUp   = ( 1, oct + 1, NoAccidental)
    | k == KeyDown = (num - 1, oct, if num - 1 `elem` acciNums then Flat  else NoAccidental)
    | k == KeyUp   = (num + 1, oct, if num + 1 `elem` acciNums then Sharp else NoAccidental)
    | otherwise    = (num, oct, NoAccidental) -- should never occur
    -- note values for sharps and flats
    where acciNums = [2, 4, 7, 9, 11]
     

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

-- main functions -------------------------------------------------------------------------------------------------------------------------------


-- Initial world state
initialWorld :: World
initialWorld = World (map noteString2noteVal ["c"]) (0)

-- EventKey Key KeyState Modifiers (Float, Float)    
handleEvent :: Event -> World -> World
handleEvent (EventKey (SpecialKey k) Down _ _) world = handleArrowKeys world k
handleEvent (        _          ) world = world

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
