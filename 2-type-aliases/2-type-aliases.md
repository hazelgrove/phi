# Motivation

**Type aliases** give users ability to define a shorthand for another type, which is either a user defined type or primitive type. As for benifits, type aliases can reduce the amount of typing as well as increasing the readability of code. 

Moreover, most of pupolar programming languages provide this feature such as `typedef` in C/C++ and [type synonym](https://wiki.haskell.org/Type_synonym) feature in Haskell. 

# Overview
TODO: provide an overview

# Proposed Changes
TODO: in all subsections, write which files you expect to change or add in the implementation. For example:

   Files to be changed:
   * `UHExp.re` -- short summary of the changes you plan to make

## Singleton Kind System
### Kind Checking
TODO: describe the kind system

### Type Equivalence
TODO: describe how type equivalence should work

## Expression Language
TODO: we are adding a new line item for type aliases

    TODO: propose the syntax
    TODO: what are the semantics

## Type-Level Language
### Type Variables
TODO: propose the change to UHTyp.t (make sure you consider invalid type variables)

    TODO: writing down the kinding rules for kind inconsistencies and invalid type variables

TODO: propose the change to HTyp.t (see polymorphism branch, Charles Chamberlain)
TODO: how will Action module change

## Pattern Language
The pattern language will not change.

## Dynamic Semantics
TODO: does this need any changes?

## User Interface
### Key Bindings
TODO: should be able to use existing mechanisms for the most part, but summarize approach

### Cursor Inspector
TODO: adding new error messages for kind inconsistencies and invalid type variables

### Pretty Printer
TODO: adding support for new constructs
