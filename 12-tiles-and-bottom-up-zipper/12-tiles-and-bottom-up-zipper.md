## Summary

The first step toward a tile-based editor will be a pure refactor (modulo fixing
some bugs or smoothing out some small but awkward ux details along the way).

This refactor will reorganize the Hazel codebase around two central concepts: a
tile-based syntax and a bottom-up zipper based edit state.

A tile-based syntax will generalize our existing `OpSeq`-based syntax to better
support prefix/postfix unary operators and unifies current structural
discrepancies across sorts (eg blocks/lines in expressions but not
types/patterns).

A bottom-up zipper based edit state, a departure from our current top-down
zipper structure (see `ZExp`/`ZPat`/`ZTyp`), will enable vastly better reuse of
type checking logic across our codebase, in particular the logic for propagating
type-contextual information down the tree. A bottom-up zipper splits the program
tree into a subject (what is currently focused) and a frame (everything else).
Having a frame datatype allows us to write the type-contextual info propagation
logic once and reuse as needed without having to write a new type-directed
recursive traversal as we currently do.
