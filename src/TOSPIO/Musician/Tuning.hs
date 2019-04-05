module TOSPIO.Musician.Tuning where

import Data.Proxy
import TOSPIO.Musician.Analog (Frequency)

class Tuning t notation where
  toFreq :: Proxy t -> notation -> Frequency
