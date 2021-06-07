# TSG-arithmetics
Implementation of + and * operators in TSG and testing reverse computation algorithm on them

`TSG-arithmetics-simple/src` contains `Plus.hs`, `Mult.hs` with implementation of the operators.
`TSG-arithmetics-simple/test` contains operators and `ura` correctness tests

**Note** ura won't finish because input of plus and mult can be any Exp, so it doesn't know that it can stop after first answer (in case we're looking for one missed argument), that's why only the first found answer is checked.

## How to build, run and test

- build: `stack build`
- run: `stack run`
- ghci: `stack ghci`
- run all tests: `stack test`

## Good to know

- `plusProg`, `multProg` - programms with plus and mult operations (they must receive binary numbers with empty ATOM (ATOM "") at the end)
- `zero`, `one`, `empty` - atoms to build numbers
- `decimalToBinaryExp` - help function which converts Int to CONS list to represent binary number
- `binaryExpToDecimal` - inverse function
- best examples of usage can be found in `TSG-arithmetics-simple/test` folder

## Results

Will be provided in presentation, but shortly: I've managed to implement two vital arithmetic operations and `ura` perfectly finds missed arguments for them (but worth to mention that for large numbers (e.g. >10e6) it can take a lot of time)
