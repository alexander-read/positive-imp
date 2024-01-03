## Overview

A parser for (the positive, implicational fragment of) intuitionistic propositional logic.

An overview of some background material is here.

Provided you have GHC, stack, etc., installed, clone this repository and run:

```powershell
➜ positive-imp git:(main) ✗ stack setup
➜ positive-imp git:(main) ✗ stack build
➜ positive-imp git:(main) stack exec positive-imp-exe
Welcome to the REPL!
To quit, type ':q'
Implication> CCpCqrCCpqCpr
((p -> (q -> r)) -> ((p -> q) -> (p -> r)))
```
