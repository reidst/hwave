# Installation Instructions
1. Ensure Haskell is installed with GHC version 9.2.6 and Cabal 3.12.1 (these
   are the only tested versions). I suggest using
   [GHCUp](https://www.haskell.org/ghcup/) for managing the Haskell toolchain.
2. The only dependency is the `bytestring` package.
3. This project is a library; it is meant to be imported elsewhere. However,
   there is also an example executable in `exe/Main.hs`. This file imports the
   library and can be run via `cabal run example`.
4. HWave programs generate RIFF WAVE files (which typically end in `.wav` or
   `.wave`). They can be run in most audio programs.
