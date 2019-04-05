module TOSPIO.Musician.Notation.Basic where

import TOSPIO.Musician.Pitch

data Degree = C | D | E | F | G | A | B

data Symbol = Natural
            | Sharp
            | Flat
            | Quantified Symbol Double

newtype PitchClass = PitchClass (Degree, [Symbol])

type Octave = Int

newtype Pitch = Pitch (PitchClass, Octave)

-- TODO: Implement this
normalize :: Pitch -> Pitch
normalize _ = Pitch (PitchClass (C, []), 4)
