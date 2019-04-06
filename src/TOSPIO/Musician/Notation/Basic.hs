module TOSPIO.Musician.Notation.Basic where

import           Data.List
import           Debug.Trace
import           Prelude                hiding (negate)
import           TOSPIO.Musician.Tuning

data Degree = C | D | E | F | G | A | B
            deriving (Show, Read, Ord, Eq, Enum)

data Symbol = Natural
            | Sharp
            | Flat
            | Quantified Symbol Double
            deriving (Show, Eq)

instance Enum Symbol where
  fromEnum Natural          = 0
  fromEnum Sharp            = 1
  fromEnum Flat             = -1
  fromEnum (Quantified _ _) = error "Quantified symbols are not supported yet"

  toEnum 0    = Natural
  toEnum 1    = Sharp
  toEnum (-1) = Flat
  toEnum n    = error $ "Invalid integer for enum Symbol: " ++ show n

newtype PitchClass = PitchClass (Degree, [Symbol])
                   deriving (Show, Eq)

type Octave = Int

newtype Pitch = Pitch (Octave, PitchClass)
              deriving (Show, Eq)

data Interval = Interval { number  :: Int
                         , quality :: Int
                         } deriving (Show, Eq)

totalDegrees :: Int
totalDegrees = 7

semitonesTable :: [Int]
semitonesTable = [0, 2, 2, 1, 2, 2, 2]

totalSemitones :: Int
totalSemitones = 12

nextDegree :: Degree -> Degree
nextDegree B = C
nextDegree d = succ d

prevDegree :: Degree -> Degree
prevDegree C = B
prevDegree d = pred d

intervalToSemitones :: Interval -> Int
intervalToSemitones Interval{..} = let
  (octaves, degInt) = number `quotRem` totalDegrees
  in octaves * totalSemitones + quality + degToSemitones degInt
  where
    degToSemitones n = case n of
      0 -> 0   -- unison
      1 -> 2   -- second
      2 -> 4   -- third
      3 -> 5   -- fourth
      4 -> 7   -- fifth
      5 -> 9   -- sixth
      6 -> 11  -- seventh
      _ | n < 0 -> -degToSemitones (-n)
      _ -> error "Impossible"


negate :: Interval -> Interval
negate Interval{..} = Interval { number = -number
                               , quality = -quality
                               }

mergeSymbols :: [Symbol] -> [Symbol]
mergeSymbols symbols
  | n >= 0 = replicate n Sharp
  | n < 0 = replicate (-n) Flat
  where n = foldl' (+) 0 $ map fromEnum symbols

infixl 7 <:+
(<:+) :: Pitch -> Interval -> Pitch
Pitch (oct, PitchClass (deg, symbols)) <:+ interval@Interval{..} = let
  (octOffset, newDeg, semitones) = addDegrees deg number

  diffSemitones = intervalToSemitones interval - semitones
  in Pitch (oct + octOffset, PitchClass (newDeg, mergeSymbols $ symbols ++ diffSemitonesToSymbols diffSemitones))
  where
    addDegrees deg number = let
      (octaves0, newDegInt0) = (fromEnum deg + number) `quotRem` totalDegrees
      (octaves, newDegInt) = if newDegInt0 < 0 then (octaves0 - 1, newDegInt0 + totalDegrees) else (octaves0, newDegInt0)
      semitones = octaves * totalSemitones + findSemitones (fromEnum deg) newDegInt
      in (octaves, toEnum newDegInt, semitones)

    findSemitones l r
      | l > r = -findSemitones r l
      | otherwise = sum $ take (r - l) $ drop (l+1) semitonesTable

    diffSemitonesToSymbols diffSemitones
      | diffSemitones < 0 = replicate (-diffSemitones) Flat
      | otherwise = replicate diffSemitones Sharp

infixl 7 <:-
(<:-) :: Pitch -> Interval -> Pitch
p <:- i = p <:+ negate i

infixl 7 +:>
(+:>) :: Interval -> Pitch -> Pitch
(+:>) = flip (<:+)

infixl 7 -:>
(-:>) :: Interval -> Pitch -> Pitch
(-:>) = flip (<:-)

absoluteSemitones :: Pitch -> Int
absoluteSemitones (Pitch (octave, PitchClass (deg, symbols))) =
  octave * totalSemitones + sum (take (fromEnum deg + 1) semitonesTable) + foldl' (+) 0 (map fromEnum symbols)

showReadable :: Pitch -> String
showReadable (Pitch (octave, PitchClass (deg, symbols))) = show deg ++ showSymbols symbols ++ show octave
  where
    showSymbol Sharp = "#"
    showSymbol Flat = "b"
    showSymbol Natural = ""
    showSymbols symbols = foldl' (++) "" $ map showSymbol symbols
