## Overview

This is a proposal to for compiler of Hazel.

## Motivation

Hazel now has only a simple evaluator but not a compiler. Therefore, a compiler for Hazel can make program run fast by our runtime design and optimizations. The compiler should also consider all incomplete programs and make sure the result also works as evaluator.

## Implementation

The implementation of compiler is now based on Grain, a functional programming language made for webassembly. The Hazel program (DHExp.t) is first transformed to Grain program. And the Grain's compiler will compile the program into webassembly.

The complete Hazel is directly transformed into Grain program. But due to restriction of Grain, there exists some case that Grain can not handle. The detail will be listed into State section.

For incomplete program, there are many problems unsolved. The detail will be discussed in [empty_hole.md](./empty_hole.md).

## State

- compile complete program
    - [ ] case expression supports int/bool/float as pattern
- compile incomplete program
    - [ ] empty hole / nonempty hole
        - [ ] basic dynamics
        - [ ] context
    - [ ] gradual type
    - [ ] pattern matching
- program analysis and optimization