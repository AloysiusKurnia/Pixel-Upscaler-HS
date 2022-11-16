A Haskell program to upscale images, primarily targeted at pixel art. This program offers three simple types of algorithm: nearest neighbor, HQX, and ScaleX.

A project by Aloysius Kurnia Mahendra and Muhamad Nicky Salim.

How to run this program:
- Without compiling to a file:
  1. Run `ghci` at this directory.
  2. Prompt `:l main.hs` on the GHCi.
  3. Resolve any compilation error, if any.
  4. If the module is loaded, you may prompt `:run main.hs`
  5. After running, make sure to delete the output file before pushing.
- With compiling to a file:
  1. Run `ghc main.hs` at this directory.
  2. Resolve any compilation error, if any.
  3. Wait for `main.exe` to appear.
  4. Run `./main.exe`.
  5. After running, make sure to delete the output file (bmp and the exe) before pushing.