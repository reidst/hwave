module Hwave where

-- | An audio channel is modeled as a value between -1 and 1 parameterized
--   by time in seconds.
type Wave = Time -> Sample

type Frequency = Double
type Time = Double
type Sample = Double

tau :: Double
tau = 2 * pi

numSquareSamples :: Double
numSquareSamples = 16

sine :: Frequency -> Time -> Wave
sine f d t = if t < d then sin (tau * f * t) else 0

square :: Frequency -> Time -> Wave
square f d t = if t < d
  then sum (map (\x -> sin (tau * f * t * (2 * x + 1))) [1..numSquareSamples]) / numSquareSamples
  else 0

delay :: Double -> Wave -> Wave
delay d w t = if t < d then 0 else w (t - d)

layer :: Wave -> Wave -> Wave
layer w v t = (w t + v t) / 2

eval :: String -> Either String [Int]
eval _ = Left "eval not implemented yet"
