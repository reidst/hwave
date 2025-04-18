module Main where

import Hwave (Wave)

import Data.ByteString          (ByteString)
import Data.ByteString.Internal (c2w)
import Data.Word                (Word8)
import qualified Data.ByteString as BS

-- | Fundamental information needed to create a PCM WAVE file.
data WaveData = WD
  { waveSampleRate :: Int     -- samples per second
  , waveBitDepth   :: Int     -- bits per sample
  , waveDuration   :: Int     -- seconds per file
  , waveGenerators :: [Wave]  -- list of channel generators
  }

main :: IO ()
main = do
  let step = 2 ** (1/12)
      a +> b = a * step ** b
      a3  = 220
      b3  = a3 +> 2
      c4  = a3 +> 3
      d4  = a3 +> 5
      e4  = a3 +> 7
      f'4 = a3 +> 9
      g4  = a3 +> 10
      a4  = a3 +> 12
      c5  = a4 +> 3
      d5  = a4 +> 5
      e5  = a4 +> 7
      f5  = a4 +> 8
      g5  = a4 +> 10
      a5  = a4 +> 12
      wd  = WD
        { waveSampleRate = 44100
        , waveBitDepth = 16
        , waveDuration = 32
        , waveGenerators =
          [ \t -> 
            let sqr f = if t * f - fromInteger (floor (t * f)) < 0.5 then 1 else 0
                tempo = 240
                beat = t * tempo / 60
                b = floor beat
                notes =
                  [  c4,  e4,  g4,  c5,  e5,  g4,  c5,  e5
                  ,  c4,  e4,  g4,  c5,  e5,  g4,  c5,  e5
                  ,  c4,  d4,  a4,  d5,  f5,  a4,  d5,  f5
                  ,  c4,  d4,  a4,  d5,  f5,  a4,  d5,  f5
                  ,  b3,  d4,  g4,  d5,  f5,  g4,  d5,  f5
                  ,  b3,  d4,  g4,  d5,  f5,  g4,  d5,  f5
                  ,  c4,  e4,  g4,  c5,  e5,  g4,  c5,  e5
                  ,  c4,  e4,  g4,  c5,  e5,  g4,  c5,  e5
                  ,  c4,  e4,  a4,  e5,  a5,  a4,  e5,  a5
                  ,  c4,  e4,  a4,  e5,  a5,  a4,  e5,  a5
                  ,  c4,  d4, f'4,  a4,  d5, f'4,  a4,  d5
                  ,  c4,  d4, f'4,  a4,  d5, f'4,  a4,  d5
                  ,  b3,  d4,  g4,  d5,  g5,  g4,  d5,  g5
                  ,  b3,  d4,  g4,  d5,  g5,  g4,  d5,  g5
                  ,  b3,  c4,  e4,  g4,  c5,  e4,  g4,  c5
                  ,  b3,  c4,  e4,  g4,  c5,  e4,  g4,  c5
                  ]
            in  sqr (if b < length notes then notes !! b else 0)
          ]
        }
  writeWaveFile "h.wave" wd

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

-- | Counts how many channels are defined.
waveChannelCount :: WaveData -> Int
waveChannelCount wd = length (waveGenerators wd)

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

-- | The number of bytes in the WAVE file header.
headerSize :: Int
headerSize = 44

-- | The size of the FMT header chunk.
fmtChunkSize :: Int
fmtChunkSize = 16

-- | The RIFF format code for PCM data.
formatCode :: Int
formatCode = 1

-- | Generates the WAVE file header.
generateHeader :: WaveData -> ByteString
generateHeader wd = foldl BS.snoc BS.empty $
     map c2w "RIFF"
  ++ toBytes 4 (waveDataSize wd + headerSize) -- file length in bytes
  ++ map c2w "WAVE"
  ++ map c2w "fmt "
  ++ toBytes 4 fmtChunkSize -- size of FMT chunk in bytes (16 for PCM) (4 bytes)
  ++ toBytes 2 formatCode -- format code (1=PCM) (2 bytes)
  ++ toBytes 2 (waveChannelCount wd) -- number of channels (2 bytes)
  ++ toBytes 4 (waveSampleRate wd) -- sampling rate (4 bytes)
  ++ toBytes 4 (waveDataRate wd) -- data rate (4 bytes)
  ++ toBytes 2 (waveBytesPerSample wd) -- bytes per sample (2 bytes)
  ++ toBytes 2 (waveBitDepth wd) -- bits per sample (2 bytes)
  ++ map c2w "data"
  ++ toBytes 4 (waveDataSize wd) -- data section length in bytes (4 bytes)

-- | Generates a single sample block.
generateSample :: WaveData -> Int -> ByteString
generateSample wd i = case waveGenerators wd of
  [gen] -> 
    let seconds = toDouble i / toDouble (waveSampleRate wd)
        sRaw = gen seconds
        sClamped = min 1 (max (-1) sRaw)
        sScaled = floor $ sClamped * (2 ^ (waveBitDepth wd - 1) - 1)
        byteSize = waveBitDepth wd `div` 8
        bytes = toBytes byteSize sScaled
    in toBS bytes
  _ -> error "the number of channels given was /= 1; only mono is supported"

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
