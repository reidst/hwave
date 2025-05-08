# HWave
_A final project in CSCI 360 Programming Languages by Katherine Reid_

## About
HWave is an embedded domain-specific language in Haskell that allows for the
creation, manipulation, and composition of frequencies, waves, and songs,
exporting created songs to a `.wav` file. Custom waveforms are modeled as pure
functions of time, and songs are modeled as waveform segments.

## Fun Features
- Music notes are built-in to the language! For example, `a 4` gives 440 Hz and
  `c' 2` gives a C# almost two octaves below middle C (it can also be written `c
  2 #> 1`, or even `c 4 <@ 2 #> 1`).
- Custom wave segments can be used as samples via the `sample` function. This
  allows for speeding up or slowing down a song, or using it as a note.
- Infix function syntax allows for readable descriptions of songs.

## Project Structure
HWave is defined in a single library file of a cabal project, which contains the
relevant type synonyms, constructors, combinators, and export functionality. The
file is divided into logical sections and fully documented.

## Reflection
I liked working on this project, although I can tell there's a lot more I could
still put into it. This is my first EDSL, so the most challenging part (even
harder than RIFF file formatting!) was finding a good structure and format for
data. I'm pretty proud of the design I came up with: being able to segment waves
via `for` and connect subsongs both in parallel via `andThen` and in series via
`and`.

## Acknowledgements
I owe thanks to Professor Peter Kabal from McGill University for [this
description of the RIFF file
format](https://mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/WAVE.html),
Bjoern Schilberg for their [How to Write a Wav File in
C](https://gist.github.com/BjoernSchilberg/c5deafaa5b3d477f60543ef59fad0a00)
GitHub gist, and http://www.topherlee.com/software/pcm-tut-wavformat.html for
its information on the WAV/RIFF file format. I should also shout out the Desmos
graphing calculator for its `tone` function which got me interested in waveform
manipulation. It was a great jumping-off point--just not quite as powerful as
Haskell. And of course, much thanks to my professor Brent Yorgey for advising
this project and teaching the course.
