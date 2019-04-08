module TOSPIO.Musician.TuningSystem.EqualTemperament where

import           Data.Proxy
import           TOSPIO.Musician.Analog
import qualified TOSPIO.Musician.Notation.IPN as IPN
import           TOSPIO.Musician.Tuning

data IPN12ET = IPN12ET IPN.Pitch Frequency

a440IPN12ET :: IPN12ET
a440IPN12ET = IPN12ET (IPN.Pitch (4, IPN.PitchClass (IPN.A, []))) 440

instance Tuning IPN12ET IPN.Pitch where
  toFreq (IPN12ET standard f) p = let
    standardSemitones = IPN.absoluteSemitones standard
    semitones = IPN.absoluteSemitones p
    in f * 2 ** (fromIntegral (semitones - standardSemitones) / 12)
