# dechorder

A library that aims to be a complete MIDI composition toolkit

## Features

* [x] A stupid pitch and interval parser
* [x] Absolute pitch and interval arithmetic
* [x] Pitch to frequency conversion under a given tuning system (now supports
      12-equal temperament)

## Guide

* Playing with pitches and intervals

``` haskell
import TOSPIO.Musician.Notation.Basic
import TOSPIO.Musician.Notation.Basic.Parser

main :: IO ()
main = do
  let p = pp "A#4"           -- Pitch (4, PitchClass (A, [Sharp]))
      majorThird = ip "3M"   -- Major third
      p' = p <:+ majorThird  -- Pitch (5, PitchClass (C, [Sharp, Sharp]))
  print $ showReadable p'    -- "C##5"
  
  print $ map showReadable $ take 12 $ iterate (<:+ ip "5") $ pp "C3"  -- Circle
  of fifths
```

* Pitch to frequency
``` haskell
import Data.Proxy
import TOSPIO.Musician.Notation.Basic
import TOSPIO.Musician.Notation.Basic.Parser
import TOSPIO.Musician.Tuning

main :: IO ()
main = do
  let p = pp "A#4"           -- Pitch (4, PitchClass (A, [Sharp]))
      majorThird = ip "3M"   -- Major third
      p' = p <:+ majorThird  -- Pitch (5, PitchClass (C, [Sharp, Sharp]))
  print $ toFreq (Proxy :: Proxy EqualTemperament) (pp "A4")    -- "440.0"
  print $ toFreq (Proxy :: Proxy EqualTemperament) (pp "C5")    -- "523.2511306011972"
```
