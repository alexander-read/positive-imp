## Overview

A parser for (the positive, implicational fragment of) intuitionistic propositional logic.

(TODO: implement condensed detachment using 'core' of Hindley-Milner algorithm).

An overview of some background material is [here](https://alexander-read.github.io/parsing-prefix.html).

Provided you have GHC, stack, etc., installed, clone this repository and run in the `/positive-imp` directory:

```powershell
$ stack setup
$ stack build
$ stack exec positive-imp-exe
Welcome to the REPL!
To quit, type ':q'
L->
```

Then, simply type any prefix formula, and you should get the infix variant:

```powershell
L-> CCpCqrCCpqCpr
=== ((p -> (q -> r)) -> ((p -> q) -> (p -> r)))
```
