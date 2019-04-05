module TOSPIO.Musician.TuningSystem.EqualTemperament where

import Data.Proxy
import TOSPIO.Musician.Analog
import TOSPIO.Musician.Pitch
import TOSPIO.Musician.Notation.Basic as BasicNotation
import TOSPIO.Musician.Notation.Yukio as YukioNotation

data EqualTemperament

instance Tuning EqualTemperament BasicNotation.Pitch where
  -- TODO: Implement this
  toFreq _ _ = 440

instance Tuning EqualTemperament YukioNotation.Pitch where
  toFreq _ _ = 440
