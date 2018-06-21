# Connection-set algebra (CSA) library

A library for constructing connection matrices between two collections of
elements. Inspired by Mikael Djurfeldt's article from 2012 ([Neuroinformatics](https://doi.org/10.1007/s12021-012-9146-1)).

## Introduction
Connection-set algebra is a powerful algebra for describing connections between
two elements.
This library provides a syntax tree for modeling the set operations, as well as
a means to transform the operations into adjacency matrices.

## Installation
This is a library and not an executable.
Clone the repository, enter it and run `stack build` (requires [stack](http://haskellstack.org/)).

## Connection-set algebra (CSA)
Say that you have two nodes that connect to each other.
In a adjacency matrix this can be described as a full connection like so:

        1 2
      + ———
    1 | 1 1
    2 | 1 1

In CSA this is simply an `AllToAll` connection.
Similarly a `OneToOne` connection describes the following adjacency matrix:

        1 2
      + ———
    1 | 1 0
    2 | 0 1

And here is the algebra part: If we say `AllToAll - OneToOne` we get:

        1 2
      + ———
    1 | 0 1
    2 | 1 0

## Contact
Jens Egholm <jensegholm@protonmail.com>
