{-# LANGUAGE PostfixOperators #-}

module Hwave where

import Data.ByteString          (ByteString)
import Data.ByteString.Internal (c2w)
import Data.Word                (Word8)
import qualified Data.ByteString as BS

-- | An audio channel is modeled as a value between -1 and 1 parameterized
--   by time in seconds.
newtype Wave a = Wave (Time -> a)
-- | Seconds
type Time = Double
-- | Sample strength in the range (-1, 1)
type Amplitude = Double

-- | Better than pi.
tau :: Double
tau = 2 * pi

------------------------------------------------------------
-- Frequency Manipulation
------------------------------------------------------------

-- | Hertz
type Frequency = Double

rawFrequency :: Double -> Frequency
rawFrequency = id

newtype TET12 = TET12 Integer



halfStep :: Double
halfStep = 2 ** (1/12)

transpose :: Integer -> Frequency -> Frequency
transpose i f = f * halfStep ** fromInteger i

transposeOctave :: Integer -> Frequency -> Frequency
transposeOctave i f = f * 2 ** fromInteger i

a4 :: Frequency
a4 = 440

(#) :: Frequency -> Frequency
(#) = transpose 1

-- | Generate a pure sine wave.
sine :: Frequency -> Wave Amplitude
sine f = Wave (\t -> sin (tau * f * t))

-- | Generate a pure square wave.
square :: Frequency -> Wave Amplitude
square f = Wave (\t -> if t * f - fromInteger (floor (t * f)) < 0.5 then 1 else 0)

-- | Silence a wave after some time.
trim :: Time -> Wave a -> Wave a
trim d (Wave w) = Wave (\t -> if t > d then 0 else w t)

-- | Translate the wave forward in time.
delay :: Time -> Wave a -> Wave a
delay d (Wave w) = Wave (\t -> if t < d then 0 else w (t - d))

-- | Evenly combines two waves atop one another.
layer :: Num a => Wave a -> Wave a -> Wave a
layer (Wave w1) (Wave w2) = Wave (\t -> w1 t + w2 t)

------------------------------------------------------------
-- Wave Generation
------------------------------------------------------------

-- | Fundamental information needed to create a PCM WAVE file.
data WaveData = WaveData
  { waveSampleRate :: Int             -- samples per second
  , waveBitDepth   :: Int             -- bits per sample
  , waveDuration   :: Int             -- seconds per file
  , waveGenerator  :: Wave Amplitude  -- sample generator
  }

-- | Basic conversion from `Int` to `Double`.
toDouble :: Int -> Double
toDouble = fromInteger . toInteger

-- | Basic conversion from `[Int]` to `ByteString`.
toBS :: [Word8] -> ByteString
toBS =
    foldl BS.snoc BS.empty

-- | Converts the latter number into a string of the former number of bytes
--   in little-endian order.
toBytes :: Int -> Int -> [Word8]
toBytes l =
    map (fromInteger . toInteger) -- conversion from Int to Word8
  . take l
  . map (`mod` 256)
  . iterate (`div` 256)

-- | Counts how many channels are defined. Currently, only mono is supported.
waveChannelCount :: WaveData -> Int
waveChannelCount _ = 1

-- | Calculates the number of bytes used for each sample (bit depth * channel
--   count / 8).
waveBytesPerSample :: WaveData -> Int
waveBytesPerSample wd = waveBitDepth wd * waveChannelCount wd `div` 8

-- | Calcuates the data rate in bytes per second (sample rate * bytes per
--   sample).
waveDataRate :: WaveData -> Int
waveDataRate wd = waveSampleRate wd * waveBytesPerSample wd

-- | Calculates the number of samples to render in total.
waveSampleCount :: WaveData -> Int
waveSampleCount wd = waveSampleRate wd * waveDuration wd

-- | Calculates the total size of the data section.
waveDataSize :: WaveData -> Int
waveDataSize wd = waveSampleCount wd * waveBytesPerSample wd

-- | The number of bytes in the WAVE file header. For PCM data, it is 44.
headerSize :: Int
headerSize = 44

-- | The size of the FMT header chunk. For PCM data, it is 16.
fmtChunkSize :: Int
fmtChunkSize = 16

-- | The RIFF format code for PCM data.
formatCode :: Int
formatCode = 1

-- | Generates the WAVE file header.
generateHeader :: WaveData -> ByteString
generateHeader wd = foldl BS.snoc BS.empty $
     map c2w "RIFF"
  -- file length in bytes
  ++ toBytes 4 (waveDataSize wd + headerSize)
  ++ map c2w "WAVE"
  ++ map c2w "fmt "
  -- size of FMT chunk in bytes (16 for PCM) (4 bytes)
  ++ toBytes 4 fmtChunkSize
  -- format code (2 bytes)
  ++ toBytes 2 formatCode
  -- number of channels (2 bytes)
  ++ toBytes 2 (waveChannelCount wd)
  -- sampling rate (4 bytes)
  ++ toBytes 4 (waveSampleRate wd)
  -- data rate (4 bytes)
  ++ toBytes 4 (waveDataRate wd)
  -- bytes per sample (2 bytes)
  ++ toBytes 2 (waveBytesPerSample wd)
  -- bits per sample (2 bytes)
  ++ toBytes 2 (waveBitDepth wd)
  ++ map c2w "data"
  -- data section length in bytes (4 bytes)
  ++ toBytes 4 (waveDataSize wd)

-- | Generates a single sample block.
generateSample :: WaveData -> Int -> ByteString
generateSample wd i =
  let seconds = toDouble i / toDouble (waveSampleRate wd)
      Wave wGen = waveGenerator wd
      sRaw = wGen seconds
      sClamped = min 1 (max (-1) sRaw)
      sScaled = floor $ sClamped * (2 ^ (waveBitDepth wd - 1) - 1)
      byteSize = waveBitDepth wd `div` 8
      bytes = toBytes byteSize sScaled
  in toBS bytes

-- | Generates all the blocks for a (mono-channel) WAVE file.
generateBlocks :: WaveData -> ByteString
generateBlocks wd =
    BS.concat
  . map (generateSample wd)
  $ [0 .. waveSampleCount wd]

writeWaveFile :: FilePath -> WaveData -> IO ()
writeWaveFile fp wd = do
  let header = generateHeader wd
      blocks = generateBlocks wd
  BS.writeFile fp $ header <> blocks
