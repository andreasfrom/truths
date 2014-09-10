# truths

A simple command line interface in Haskell for examining formulas in propositional logic by generating and cross-checking truth tables.

# Sample session

It's easiest to just show the functionality with a picture:

![Sample session](https://raw.githubusercontent.com/andreasfrom/truths/master/session.png)

# Download

I hope to generate some binaries some time soon.

# Conventions

I've adopted a strict precedence hierarchy for the logical connectives to simplify parsing and printing.

The operators are presented here from highest to lowest precedence:

- Negation: ¬
- Conjunction: ∧
- Disjunction: ∨
- Exclusive disjunction: ⊕
- Implication: ⇒
- Biconditional: ⇔

It must be noted that implication is right associative so `p ⇒ q ⇒ r` is parsed as `p ⇒ (q ⇒ r)`.
The rest are associative.

To give a (contrived) example, the proposition: `¬p∧q v r ⇔ q` is parsed as `(((¬p)∧q) v r) ⇔ q`

# Code Notes

The parser, expression evaluation and half of the truth table logic is very nice code.
The cli and equality code on the other hand is best left unseen; I hope to clean it up eventually.

Anyhow, there are only 174 lines of code, so it should be quite easy to understand how it works.

# Copyright and license
Copyright (c) 2014 Andreas H. From

Licensed under the MIT License (see the file LICENSE)

