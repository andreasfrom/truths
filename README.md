# truths

A simple command line interface in Haskell for examining formulas in propositional logic by generating truth tables and semantic tableaus.

# Demo

I've created a small video showcasing generating truth tables and semantic tableaus as well as checking if two propositions are equivalent with the tool:

[![YouTube](https://i.ytimg.com/vi/dWPhAmp3SGc/0.jpg?time=1)](http://youtu.be/dWPhAmp3SGc)

# Commands

- `<formula>`: Just entering a plain formula prints its truth table
- `latex <formula>` generates the table in LaTeX-format with the `tabular` environment so you can copy/paste it into a LaTeX document.
- `context <formula>` generates a truth table for use in ConTeXt documents.
- `tab[leau] <formula> t/f/T/F/1/0`, for instance `tab pv¬p f` generates an image called `semtab.svg` with the formula's corresponding semantic tableau

# Download

I unfortunately can't seem to produce a working binary on anything but my own computer.
The easiest thing is probably to compile the code yourself at the moment, unfortunately.

# Copyright and license
Copyright (c) 2014 Andreas H. From

Licensed under the MIT License (see the file LICENSE)
