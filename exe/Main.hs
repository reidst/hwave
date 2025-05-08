import Hwave
import Prelude hiding (and)

-- Prelude in C Major by J. S. Bach
-- This song is used as an example due to its exceptionally structured melody.

-- tempo data
tempo :: Frequency
tempo = 80

whole, half, quarter, eighth, sixteenth :: Frequency
whole = quarter * 4
half = quarter * 2
quarter = 60 / tempo
eighth = quarter / 2
sixteenth = quarter / 4

-- the "instrument" to use
waveType :: Frequency -> Wave
waveType f = sine f ~+~ square f

-- the first 32 measures, compressed
notes :: [[Frequency]]
notes =
  [ [c 4, e 4, g 4, c 5, e 5] -- measure 1
  , [c 4, d 4, a 4, d 5, f 5]
  , [b 3, d 4, g 4, d 5, f 5]
  , [c 4, e 4, g 4, c 5, e 5] -- 4
  , [c 4, e 4, a 4, e 5, a 5]
  , [c 4, d 4, f' 4,a 4, d 5]
  , [b 3, d 4, g 4, d 5, g 5] -- 7
  , [b 3, c 4, e 4, g 4, c 5]
  , [a 3, c 4, e 4, g 4, c 5]
  , [d 3, a 3, d 4, f' 4,c 5] -- 10
  , [g 3, b 3, d 4, g 4, b 4]
  , [g 3, a' 3,e 4, g 4, c' 5]
  , [f 3, a 3, d 4, a 4, d 5] -- 13
  , [f 3, g' 3,d 4, f 4, b 4]
  , [e 3, g 3, c 4, g 4, c 5]
  , [e 3, f 3, a 3, c 4, f 4] -- 16
  , [d 3, f 3, a 3, c 4, f 4]
  , [g 2, d 3, g 3, b 3, f 4]
  , [c 3, e 3, g 3, c 4, e 4] -- 19
  , [c 3, g 3, a' 3,c 4, e 4]
  , [f 2, f 3, a 3, c 4, e 4]
  , [f' 2,c 3, a 3, c 4, d' 4] -- 22
  , [g' 2,f 3, b 3, c 4, d 4]
  , [g 2, f 3, g 3, b 3, d 4]
  , [g 2, e 3, g 3, c 4, e 4] -- 25
  , [g 2, d 3, g 3, c 4, f 4]
  , [g 2, d 3, g 3, b 3, f 4]
  , [g 2, d' 3,a 3, c 4, f' 4] -- 28
  , [g 2, e 3, g 3, c 4, g 4]
  , [g 2, d 3, g 3, c 4, f 4]
  , [g 2, d 3, g 3, b 3, f 4] -- 31
  , [c 2, c 3, g 3, a' 3,e 4]
  ]

-- the last 3 measures
ending1, ending2, ending3 :: Song
ending1 =
  waveType (c 2) `for` whole
  `and`
  rest `for` sixteenth `andThen` waveType (c 3) `for` sixteenth * 15
  `and`
  rest `for` eighth
  `andThen` foldl1 andThen (
    map ((`for` sixteenth) . waveType)
      [f 3, a 3, c 4, f 4, c 4, a 3, c 4, a 3, f 3, a 3, f 3, d 3, f 3, d 3]
  )
ending2 =
  waveType (c 2) `for` whole
  `and`
  rest `for` sixteenth `andThen` waveType (b 2) `for` sixteenth * 15
  `and`
  rest `for` eighth
  `andThen` foldl1 andThen (
    map ((`for` sixteenth) . waveType)
      [g 4, b 4, d 5, f 5, d 5, b 4, d 5, b 4, g 4, b 4, d 4, f 4, e 4, d 4]
  )
ending3 = chord (map waveType [ c 2, c 3, e 4, g 4, c 5]) `for` whole * 1.5

-- play a list of notes in an arpeggio
arpeggio :: [Wave] -> Time -> Song
arpeggio []     _   = rest `for` 0
arpeggio (w:ws) dur = w `for` dur `andThen` arpeggio ws dur

-- uncompress a measure
playMeasure :: [Wave] -> Song
playMeasure [w1, w2, w3, w4, w5] =
  let arp = arpeggio [w3, w4, w5] sixteenth
      part =
        w1 `for` half
        `and`
        rest `for` sixteenth `andThen` w2 `for` sixteenth * 7
        `and`
        rest `for` eighth `andThen` arp `andThen` arp
  in part `andThen` part

main :: IO ()
main = makeSong "h.wave" $
  foldl1 andThen (map (playMeasure . map waveType) notes)
  `andThen` ending1
  `andThen` ending2
  `andThen` ending3
