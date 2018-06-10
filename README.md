# Connetion-set algebra (CSA) library

A library for constructing connection matrices between two collections of
elements. Inspired by Mikael Djurfejdt's article from 2012 ([Neuroinformatics](https://doi.org/10.1007/s12021-012-9146-1)).

## Introduction
Connection-set algebra is a powerful descriptor for connections between two
collections.
Say that you have a graph of two nodes that connects to another graph of
two nodes.
In a connection matrix this can be described as a full connection like so:

        1 2
      + ———
    1 | 1 1
    2 | 1 1

## Setup
