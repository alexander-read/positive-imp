## Overview

We consider the positive implicational fragment of the intuitionistic propositional calculus.
The language is defined by the following BNF grammar, assuming an infinite set of variables $\\{p_{0}, \ p_{00}, \ p_{000}, {\dots}\\}$, and where ${\phi},{\psi}$ are meta-variables ranging over formulae:
$${\phi},{\psi} ::= p \ \vert \ {\phi} \to {\psi}$$

In practice, we use $p, q,{\dots},z$ for variables, and assume that implication associates to the right.

Following the convention set by Łukasiewicz, Meredith, Prior, and others [^3], a lot of work in this area uses prefix, rather than infix, notation to write formulae.
For example, using $C$ for the prefix-style implication operator, the following formulae correspond to the types of the combinators $\mathsf{\bf{S}} \equiv {\lambda}xyz.xz(yz)$ and $\mathsf{\bf{K}} \equiv {\lambda}xy.x$:

$$\begin{align}
  CCpCqrCCpqCpr \ {\equiv} \ &(p \to (q \to r)) \to ((p \to q) \to (p \to r)) \tag{S} \\
  CpCqp         \ {\equiv} \ &(p \to (q \to p)). \tag{K}
\end{align}$$

I find the prefix notation unintuitive to read, so I wanted to parse valid prefix strings and then print the resulting ASTs in infix format.
Rather than use an existing library (e.g., [parsec](https://hackage.haskell.org/package/parsec)), I thought this would be a good opportunity to implement the parser by hand, following the tutorials from Hutton and Meijer [^1],[^2].

This can be run from the command-line as follows:

```powershell
➜ positive-imp git:(main) ✗ stack setup  # only need to run once
➜ positive-imp git:(main) ✗ stack build  # only need to run once
➜ positive-imp git:(main) ✗ stack exec positive-imp-exe
CCpCqrCCpqCpr
((p -> (q -> r)) -> ((p -> q) -> (p -> r)))
```

## References
[^1]: Hutton, G., and Meijer, E. (1996). [Monadic Parser Combinators](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf). Technical report NOTTCS-TR-96-4, 1-38
[^2]: Hutton, G., and Meijer, E. (1998). [Monadic parsing in Haskell](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/monadic-parsing-in-haskell/E557DFCCE00E0D4B6ED02F3FB0466093). _JFP_, 8(4), 437-444
[^3]: Lemmon, E. J., Meredith, C. A., Meredith, D., Prior, A. N., and Thomas, I. (1957). [_Calculi of Pure Strict Implication_](https://link.springer.com/chapter/10.1007/978-94-010-9614-0_17), 215–250
