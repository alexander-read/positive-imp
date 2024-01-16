## Overview

A parser for (the positive, implicational fragment of) intuitionistic propositional logic.

(TODO: implement condensed detachment using 'core' of Hindley-Milner algorithm).

An overview of some background material is [here](https://alexander-read.github.io/parsing-prefix.html).

Provided you have GHC, stack, etc., installed, clone this repository and run:

```powershell
➜ positive-imp git:(main) ✗ stack setup
➜ positive-imp git:(main) ✗ stack build
➜ positive-imp git:(main) stack exec positive-imp-exe
Welcome to the REPL!
To quit, type ':q'
Implication>
```

Then, simply type any prefix formula, and you should get the infix variant:

```powershell
Implication> CCpCqrCCpqCpr
((p -> (q -> r)) -> ((p -> q) -> (p -> r)))
```
