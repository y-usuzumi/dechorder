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

degreeSemitones :: Degree -> Int
degreeSemitones p = case p of
  C -> 0
  D -> 2
  E -> 4
  F -> 5
  G -> 7
  A -> 9
  B -> 11

totalSemitones :: Int
totalSemitones = 12

negate :: Interval -> Interval
negate Interval{..} = Interval { number = -number
                               , quality = -quality
                               }

mergeSymbols :: [Symbol] -> [Symbol]
mergeSymbols symbols
  | n >= 0 = replicate n Sharp
  | n < 0 = replicate (-n) Flat
  where n = foldl' (+) 0 $ map fromEnum symbols

absoluteDegrees :: Pitch -> Int
absoluteDegrees (Pitch (octave, PitchClass (deg, _))) =
  octave * totalDegrees + fromEnum deg

absoluteSemitones :: Pitch -> Int
absoluteSemitones (Pitch (octave, PitchClass (deg, symbols))) =
  octave * totalSemitones + degreeSemitones deg + foldl' (+) 0 (map fromEnum symbols)

intervalSemitones :: Interval -> Int
intervalSemitones Interval{..} = let
  (oct, deg) = number `quotRem` totalDegrees
  in oct * 12 + signum deg * degreeSemitones (toEnum $ abs deg) + quality

diffSemitonesToSymbols :: Int -> [Symbol]
diffSemitonesToSymbols diffSemitones
  | diffSemitones < 0 = replicate (-diffSemitones) Flat
  | otherwise = replicate diffSemitones Sharp

showReadable :: Pitch -> String
showReadable (Pitch (octave, PitchClass (deg, symbols))) = show deg ++ showSymbols symbols ++ show octave
  where
    showSymbol Sharp = "#"
    showSymbol Flat = "b"
    showSymbol Natural = ""
    showSymbols symbols = foldl' (++) "" $ map showSymbol symbols

infixl 7 <:+
(<:+) :: Pitch -> Interval -> Pitch
p@(Pitch (oct, PitchClass (deg, symbols))) <:+ interval@Interval{..} = let
  newAbsDeg = absoluteDegrees p + number
  (newOct, newDegInt) = newAbsDeg `quotRem` totalDegrees
  absSemitones = absoluteSemitones $ Pitch (newOct, PitchClass (toEnum newDegInt, []))
  newAbsSemitones = absoluteSemitones p + intervalSemitones interval
  in Pitch (newOct, PitchClass (toEnum newDegInt, diffSemitonesToSymbols $ newAbsSemitones - absSemitones))

infixl 7 <:-
(<:-) :: Pitch -> Interval -> Pitch
p <:- i = p <:+ negate i

infixl 7 +:>
(+:>) :: Interval -> Pitch -> Pitch
(+:>) = flip (<:+)

infixl 7 -:>
(-:>) :: Interval -> Pitch -> Pitch
(-:>) = flip (<:-)
