module Hwave
  -- Types
  ( Wave
  , Song
  , Frequency
  , Time
  -- Waves
  , sine
  , square
  , rest
  , sample
  , chord
  -- Combinators
  , (~+~), (~*~), (~*)
  , for, (%)
  , andThen, (|>)
  , and, (<|>)
  -- Misc.
  , (#>), (<#)
  , (@>), (<@)
  , a, b, c, d, e, f, g
  , a', b', c', d', e', f', g'
  , makeSong
  ) where

import Prelude hiding (and)
import Data.ByteString          (ByteString)
import Data.ByteString.Internal (c2w)
import Data.Word                (Word8)
import qualified Data.ByteString as BS

------------------------------------------------------------
-- Types
------------------------------------------------------------

-- | An audio channel is modeled as a value between -1 and 1 parameterized
--   by time in seconds.
type Wave = Time -> Amplitude
-- | Sample strength in the range (-1, 1)
type Amplitude = Double
-- | Seconds
type Time = Double
-- | Inverse-seconds
type Frequency = Double
-- | A piece is a wave of fixed duration. A weight is also given to ensure
--   pieces can be layered properly.
type Song = (Wave, Time, Double)

------------------------------------------------------------
-- Waves
------------------------------------------------------------

-- | Generate a pure sine wave.
sine :: Frequency -> Wave
sine hz t = sin (2 * pi * hz * t)

-- | Generate a pure square wave.
square :: Frequency -> Wave
square hz t = if t * hz - fromInteger (floor (t * hz)) < 0.5 then 1 else -1

-- | A rest is a note with no volume. Useful for delaying a song.
rest :: Wave
rest _ = 0

-- | Create a wave by using a song as a sample.
sample :: Song -> Frequency -> Wave
sample (w,dur,_) hz t = w (dur * hz * t)

-- | Play a collection of waves, averaging their amplitudes.
chord :: [Wave] -> Wave
chord [] _ = 0
chord ws t = let as = map ($ t) ws in sum as / fromIntegral (length as)

------------------------------------------------------------
-- Combinators
------------------------------------------------------------

-- | Averages two waves' amplitudes.
(~+~) :: Wave -> Wave -> Wave
w1 ~+~ w2 = \t -> (w1 t + w2 t) / 2

-- | Multiplies two waves' amplitudes. Useful for applying envelopes.
(~*~) :: Wave -> Wave -> Wave
w1 ~*~ w2 = \t -> w1 t * w2 t

-- | Multiplies a wave by a value. Ideally, the value is in the range (-1, 1).
(~*) :: Wave -> Double -> Wave
w ~* x = \t -> w t * x

-- | Play a wave for a set duration.
infix 3 `for`, %
for, (%) :: Wave -> Time -> Song
for w dur = (w, dur, 1)
(%) = for

-- | Play two songs in series.
infixl 2 `andThen`, |>
andThen, (|>) :: Song -> Song -> Song
andThen (w1, d1, _) (w2, d2, _) =
  ( \t -> (if t > d1 then w2 (t - d1) else w1 t)
  , d1 + d2
  , 1
  )
(|>) = andThen

-- | Play two songs simultaneously.
infixl 1 `and`, <|>
and, (<|>) :: Song -> Song -> Song
and (w1, d1, g1) (w2, d2, g2) =
  ( \t -> 
    ( (if t < d1 then w1 t else 0) * g1
    + (if t < d2 then w2 t else 0) * g2
    ) / (g1 + g2)
  , max d1 d2
  , g1 + g2
  )
(<|>) = and

------------------------------------------------------------
-- Misc.
------------------------------------------------------------

-- | Transpose a frequency (in 12 TET).
(#>), (<#) :: Frequency -> Double -> Frequency
hz #> t = hz * (2 ** (t/12))
hz <# t = hz #> (-t)

-- | Transpose a freqency by octaves.
(@>), (<@) :: Frequency -> Double -> Frequency
hz @> t = hz #> (12 * t)
hz <@ t = hz <# (12 * t)

-- | Generate a WAVE file for a given song.
makeSong :: FilePath -> Song -> IO ()
makeSong fp (w, dur, _) = writeWaveFile fp $ WaveData
  { waveBitDepth = 24
  , waveDuration = dur
  , waveGenerator = w
  }

-- | Notes from 12 TET parameterized by octave. `a 4 = 440`.
a, b, c, d, e, f, g :: Int -> Frequency
a n = 440 @> fromIntegral (n - 4)
b n = a n #> 2
c n = a n <# 9
d n = a n <# 7
e n = a n <# 5
f n = a n <# 4
g n = a n <# 2

-- | Sharp variants of notes.
a', b', c', d', e', f', g' :: Int -> Frequency
a' n = a n #> 1
b' n = b n #> 1
c' n = c n #> 1
d' n = d n #> 1
e' n = e n #> 1
f' n = f n #> 1
g' n = g n #> 1

------------------------------------------------------------
-- Wave Generation
------------------------------------------------------------

-- | Fundamental information needed to create a PCM WAVE file.
data WaveData = WaveData
  { waveBitDepth   :: Int     -- bits per sample
  , waveDuration   :: Double  -- seconds per file
  , waveGenerator  :: Wave    -- sample generator
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

-- | The number of samples per second.
sampleRate :: Int
sampleRate = 44100

-- | Calcuates the data rate in bytes per second (sample rate * bytes per
--   sample).
waveDataRate :: WaveData -> Int
waveDataRate wd = sampleRate * waveBytesPerSample wd

-- | Calculates the number of samples to render in total.
waveSampleCount :: WaveData -> Int
waveSampleCount wd = floor $ fromIntegral sampleRate * waveDuration wd

-- | Calculates the total size of the data section.
waveDataSize :: WaveData -> Int
waveDataSize wd = waveSampleCount wd * waveBytesPerSample wd

-- | The number of bytes in the file.
fileSize :: WaveData -> Int
fileSize wd = waveDataSize wd + headerSize

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
  ++ toBytes 4 (fileSize wd)
  ++ map c2w "WAVE"
  ++ map c2w "fmt "
  -- size of FMT chunk in bytes (16 for PCM) (4 bytes)
  ++ toBytes 4 fmtChunkSize
  -- format code (2 bytes)
  ++ toBytes 2 formatCode
  -- number of channels (2 bytes)
  ++ toBytes 2 (waveChannelCount wd)
  -- sampling rate (4 bytes)
  ++ toBytes 4 sampleRate
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
  let seconds  = toDouble i / toDouble sampleRate
      wGen     = waveGenerator wd
      sRaw     = wGen seconds
      sClamped = min 1 (max (-1) sRaw)
      sScaled  = floor $ sClamped * (2 ^ (waveBitDepth wd - 1) - 1)
      byteSize = waveBitDepth wd `div` 8
      bytes    = toBytes byteSize sScaled
  in toBS bytes

-- | Generates all the blocks for a (mono-channel) WAVE file.
generateBlocks :: WaveData -> ByteString
generateBlocks wd =
    BS.concat
  . map (generateSample wd)
  $ [0 .. waveSampleCount wd]

-- | Writes a WAVE file.
writeWaveFile :: FilePath -> WaveData -> IO ()
writeWaveFile fp wd = do
  let header = generateHeader wd
      blocks = generateBlocks wd
  BS.writeFile fp $ header <> blocks
  putStrLn $
    show (fileSize wd)
    ++ " bytes ("
    ++ show (waveDuration wd)
    ++ " seconds) written to "
    ++ fp
