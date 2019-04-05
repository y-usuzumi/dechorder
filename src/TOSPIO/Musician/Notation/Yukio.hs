module TOSPIO.Musician.Notation.Yukio where

import TOSPIO.Musician.Pitch

-- The reason why I and L is missing is they look similar to the digit 1.
-- The reason why N is chosen instead of M is due to its smaller width in apprearance.
data Degree = A | B | C | D | E | F | G | H | J | K | N

type Octave = Int

newtype Pitch = Pitch (Degree, Octave)

-- TODO: Implement this
normalize :: Pitch -> Pitch
normalize _ = Pitch (A, 0)
