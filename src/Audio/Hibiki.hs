{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Audio.Hibiki where

import Data.IORef
import Control.Monad
import Control.Concurrent
import Data.Int (Int16)
import Data.Foldable
import SDL
import qualified Data.Vector.Storable.Mutable as V
import qualified Control.Concurrent.BoundedChan as C
import Control.Arrow
import Streamly
import qualified Streamly.Prelude as S

import qualified Data.Audio as A
import qualified Data.Array.IArray as A
import qualified Data.Array.Unboxed as A

import Codec.Wav

audioCB :: C.BoundedChan Int16 -> AudioFormat sampleType -> V.IOVector sampleType -> IO ()
audioCB samples format buffer =
  case format of
    Signed16BitLEAudio ->
      do
         let n = V.length buffer
         -- print n
         traverse_ (\ptr -> V.write buffer ptr =<< C.readChan samples) [0..(n -1)]
    _ -> error "Unsupported audio format"


play :: SoundSample -> IO ()
play s = do
  initializeAll
  chan <- C.newBoundedChan $ 4096 * 16
  let
    defaultDevice = OpenDeviceSpec
      { SDL.openDeviceFreq = Mandate (fromIntegral $ samplingRate s)
      , SDL.openDeviceFormat = Mandate Signed16BitNativeAudio
      , SDL.openDeviceChannels = Mandate Mono
      , SDL.openDeviceSamples = 4096 * 2
      , SDL.openDeviceCallback = audioCB chan
      , SDL.openDeviceUsage = ForPlayback
      , SDL.openDeviceName = Nothing
      }
  (device, _) <- openAudioDevice defaultDevice
  setAudioDevicePlaybackState device Play
  let sound = sampling s
  runStream $ S.mapM (C.writeChan chan) sound

data Sound = Sound
  { amplitude :: Double -> Double -- from time to (0,1) pcm
  , duration :: Double -- in seconds
  }

data SoundSample = SoundSample
  { sampling :: Serial Int16
  , samplingRate :: Int
  }

instance Semigroup SoundSample where
  s1 <> s2 = SoundSample
    { sampling = sampling s1 <> sampling s2
    , samplingRate = samplingRate s1
    }

fromSound
  :: Int -- sample rate (Hz)
  -> Sound
  -> SoundSample
fromSound rate sound = SoundSample
  { sampling = S.map (\t'-> rounder $ f (fromIntegral t' / fromIntegral rate)) timeProducer
  , samplingRate = rate
  }
     where
       timeProducer = S.unfoldr (\t -> if t < totalSample then Just (t, t + 1) else Nothing) 0
       totalSample = if duration sound == (infinity :: Double) then (maxBound :: Int) else round $ duration sound * fromIntegral rate
       f = amplitude sound


rounder :: forall s a. (Bounded a, Integral a, Floating s, RealFrac s) => s -> a
rounder s = round $ s * fromIntegral (maxBound :: a)
-- ??? if > 0 then -1 else + 1

infinity :: Read s => s
infinity = read "Infinity"

instance Num Sound where
  -- s1 + s2 = if duration s1 < duration s2 then s2 + s1 else Sound (\s -> if s < duration s2 then amplitude s1 s + amplitude s2 s else amplitude s1 s) $ duration s1
  s1 + s2 = Sound ((+) <$> amplitude s1 <*> amplitude s2) $ duration s1 `min` duration s2
  -- s1 * s2 = if duration s1 < duration s2 then s2 * s1 else Sound (\s -> if s < duration s2 then amplitude s1 s * amplitude s2 s else amplitude s1 s) $ duration s1
  s1 * s2 = Sound ((*) <$> amplitude s1 <*> amplitude s2) $ duration s1 `min` duration s2
  abs s = Sound (abs . amplitude s) $ duration s
  signum s = Sound (signum . amplitude s) $ duration s
  fromInteger i = Sound (const $ fromInteger i) infinity
  negate s = Sound (negate . amplitude s) $ duration s

instance Fractional Sound where
  s1 / s2 = Sound ((/) <$> amplitude s1 <*> amplitude s2) $ duration s1 `min` duration s2
  fromRational i = Sound (const $ fromRational i) infinity

freqSound :: (Double -> Double -> Double) -> Double -> Sound
freqSound f freq = Sound (f freq) infinity

sine :: Double -> Double -> Double
sine freq t = sin $ 2 * pi * t * freq

cosine :: Double -> Double -> Double
cosine freq t = cos $ 2 * pi * t * freq

unFourier :: Fourier -> (Double -> Double)
unFourier f t = sum $ (\(freq, a) -> a * cosine freq t) <$> f t

chord :: Synth -> Fourier -> (Double -> Double)
chord synth c t = sum $ (\(freq, a) -> a * unFourier (synth freq) t) <$> c t

type Fourier = Double -> [(Double, Double)]
type Synth = Double -> Fourier

major :: Synth
major freq = const $ zipWith (\f a -> (f * freq, a)) [2/2, 3/2, 4/2] [0.3, 0.3, 0.3]

minor :: Synth
minor freq = const $ zipWith (\f a -> (f * freq, a)) [3/3, 4/3, 5/3] [0.3, 0.3, 0.3]

instance Semigroup Sound where
  s1 <> s2 = Sound (\t -> if t < duration s1 then amplitude s1 t else amplitude s2 t) $ duration s1 + duration s2

instance Monoid Sound where
  mempty = Sound (const 0) 0

silence :: Double -> Sound
silence = Sound (const 0)

padStart :: Double -> Sound -> Sound
padStart t s = silence t <> s

padEnd :: Double -> Sound -> Sound
padEnd t s = s <> silence t

dropStart :: Double -> Sound -> Sound
dropStart t s = Sound (amplitude s . (+ t)) $ duration s - t

dropEnd :: Double -> Sound -> Sound
dropEnd t s = Sound (amplitude s) $ duration s - t

takeStart :: Double -> Sound -> Sound
takeStart t s = Sound (amplitude s) t

testSound1 = fromSound 48000 $ dropEnd 1 (Sound (\x -> sin (x * x * 440 * 6)) 12) <> takeStart 2 (freqSound cosine 440)
testSound2 = fromSound 48000 $ takeStart 10 $ Sound (unFourier $ minor 220) 30000 <> Sound (unFourier $ major 220) 4

fromAudio :: A.Audio Int16 -> SoundSample
fromAudio a = SoundSample
  { sampling = S.fromFoldable $ A.elems $ A.sampleData a
  , samplingRate = A.sampleRate a
  }


test1 = do
  (Right a) <- importFile "res/sound.wav"
  print a
  play $ fromAudio a

