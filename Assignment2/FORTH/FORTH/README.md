I found the only way to run the test cases in `EvalSpec.hs` on my PC is:

- In git bash terminal, run `runhaskell EvalSpec.hs`
- In ghci, execute `:load EvalSpec.hs`
- In ghci, execute `main`


To run the test files t1.4TH up to t10.4TH, run the following command:
- `cabal run FORTH -- tests/t1.4TH`
  - Replace file name depending on test

Expected outputs are found in thge corresponding output files located within the tests directory.