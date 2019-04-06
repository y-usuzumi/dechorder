module TOSPIO.Musician.TuningSystem.EqualTemperament where

import           Data.Proxy
import           TOSPIO.Musician.Analog
import qualified TOSPIO.Musician.Notation.Basic as BN
import qualified TOSPIO.Musician.Notation.Yukio as YN
import           TOSPIO.Musician.Tuning

data EqualTemperament

a440Semitones :: Int
a440Semitones = BN.absoluteSemitones (BN.Pitch (4, BN.PitchClass (BN.A, [])))

instance Tuning EqualTemperament BN.Pitch where
  toFreq _ p = let
    semitones = BN.absoluteSemitones p
    in 440 * 2 ** (fromIntegral (semitones - a440Semitones) / 12)

instance Tuning EqualTemperament YN.Pitch where
  toFreq _ _ = 440
