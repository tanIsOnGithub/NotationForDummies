module Draw (
    drawWorld,
	wSizeX, wSizeY,
	wCenteredX, wCenteredY,
) where

import Types ( World ( World )
             , NoteValue (noteNumber, accidental, octave, noteLength, isDotted)
			 , Accidental (Flat, Sharp, NoAccidental))
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- spacing constants ----------------------------------------------------------------------------------------------------------------------------

-- window
wSizeX     :: Float; wSizeY     :: Float; (wSizeX    , wSizeY    ) = (3200.0, 1800.0) -- window sizes in both dimensions as FLOATS
wCenteredX :: Float; wCenteredY :: Float; (wCenteredX, wCenteredY) = ((-wSizeX) / 2, (-wSizeY) / 2)

-- notation
gapBetweenStaves  :: Float; gapBetweenStaves  = wSizeY / 60
topperToSystemGap :: Float; topperToSystemGap = wSizeY * 4 / 15        -- difference between the top of the window and the highest staff line
startOfNotes      :: Float; startOfNotes      = wSizeX / 32            -- x-coordinate of the beginning of notes on a staff line
spaceBetweenNotes :: Float; spaceBetweenNotes = gapBetweenStaves * 2.5 -- horizontal space between two notes
noteHeadRadius    :: Float; noteHeadRadius    = gapBetweenStaves / 2   -- radius of a notehead which is approximated by a circle
noteStemWidth     :: Float; noteStemWidth     = noteHeadRadius / 3.5   -- width of a note stem
pixelRoundingErrorOffset :: Float; pixelRoundingErrorOffset = 7 -- aligns the noteheads perfectly inside of drawNotation
-- coordinates of the position of c in the fourth octave (c''), mCx = -1.500, mCy = 375
mCx :: Float; mCx = (-wSizeX) / 2 + startOfNotes --
mCy :: Float; mCy = wSizeY / 2 - topperToSystemGap - noteHeadRadius / 2 - noteHeadRadius * 2 - pixelRoundingErrorOffset

-- color of selected notes
selectionColor :: Color; selectionColor = violet 

-- accidentals
gabBetweenNoteAndAcci :: Float; gabBetweenNoteAndAcci = spaceBetweenNotes / 8 -- needs to be dynamic later (good luck)
flatHeight  :: Float; flatHeight  = (2 + 0.1) * gapBetweenStaves
sharpHeight :: Float; sharpHeight = (2 + 0.5) * gapBetweenStaves
flatWidth   :: Float; flatWidth   = 1.8 * noteHeadRadius
sharpWidth  :: Float; sharpWidth  = 2.0 * noteHeadRadius

-- helper functions 


-- draws a thick line 
thickLine :: Point -> Point -> Float -> Picture
thickLine a@(x1, y1) d@(x2, y2) thick = polygon [a, b, c, d]
    where (ax, ay) = computeAdjust (x2 - x1) (y2 - y1)
          -- straight lines cannot be computed with a trigonometric formula due
          -- to double precision issues
          computeAdjust  _  0 = (0, (-thick))
          computeAdjust  0  _ = (thick,    0)
          computeAdjust dx dy = (thick * cos angle, thick * sin angle) where angle = pi / 2 - (atan2 dy dx)
          b = (x1 + ax, y1 + ay)
          c = (x2 + ax, y2 + ay)

-- main functions 

drawNotation :: [NoteValue] -> Int -> Int -> [Picture]
drawNotation [                   ] _ _ = []
drawNotation (noteVal  : notes) i selectionIndex = 
    (color noteColor . pictures $ [noteStem, noteHead, noteAccidental]) : drawNotation notes (i + 1) selectionIndex
    where x = mCx + fromIntegral i * spaceBetweenNotes
          y = mCy + adjustNoteNum curNoteNum * noteHeadRadius + octY
          octY = fromIntegral (curNoteOct - 4) * noteHeadRadius * 7 -- adjusting for octave
          -- converting the raw noteNumber (character to number) to 
          -- the visual gap from middle c (adjusting for accidentals)
          -- differenciating between sharps and flats
          adjustNoteNum n = acciConvertList !! (n - 1)
          acciConvertList = if curNoteAcci == Sharp 
                            then [0, 0, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6]
                            -- also works for NoAccidental
                            else [0, 1, 1, 2, 2, 3, 4, 4, 5, 5, 6, 6] 
          -- note stem, head and color
          noteStem  = if curNoteOct >= 4 || (curNoteOct == 3 && curNoteNum == 12) then noteStemDown else noteStemUp 
          noteStemDown  = thickLine (x - noteHeadRadius, y) 
                                    (x - noteHeadRadius, y - gapBetweenStaves * 3.5) noteStemWidth
          noteStemUp    = thickLine (x + noteHeadRadius - noteStemWidth, y) 
                                    (x + noteHeadRadius - noteStemWidth, y + gapBetweenStaves * 3.5) noteStemWidth
          noteHead  = translate x y . circleSolid $ noteHeadRadius
          noteColor = if i == selectionIndex then selectionColor else white
          -- accidentals
          noteAccidental = case curNoteAcci of
                             Sharp        -> sharp
                             Flat         -> flat
                             NoAccidental -> Blank
          fx = x - noteHeadRadius - gabBetweenNoteAndAcci - flatWidth
          fy = y - noteHeadRadius
          flat  = line [(fx, fy + flatHeight), (fx, fy), (fx + flatWidth, fy + noteHeadRadius), (fx, fy + noteHeadRadius * 2)]
          sharp = Blank
          -- current note properties
          (curNoteNum, curNoteOct, curNoteAcci) = (noteNumber noteVal, octave noteVal, accidental noteVal)

drawWorld :: World -> Picture
drawWorld (World w selectionIndex) = pictures $ staveLines : drawNotation w 0 selectionIndex
   where staveLines = color white . pictures . map makeLine $ staveLinesPositions
         makeLine ly = line [(wCenteredX, ly), (wCenteredX + wSizeX, ly)] 
         -- drawing the staff lines from top to bottom
         staveLinesPositions = map (\n -> wSizeY / 2 - topperToSystemGap - n * gapBetweenStaves) [0..4]
