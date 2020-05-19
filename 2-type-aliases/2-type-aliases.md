# Motivation

**Type aliases** give users ability to define a shorthand for another type, which is either a user defined type or primitive type. As for benifits, type aliases can reduce the amount of typing as well as increasing the readability of code. 

Moreover, most of pupolar programming languages provide this feature such as `typedef` in C/C++ and [type synonym](https://wiki.haskell.org/Type_synonym) feature in Haskell. 

# Overview
TODO: provide an overview

# Proposed Changes

## Singleton Kind System
### Kind Checking
TODO: describe the kind system

### Type Equivalence
TODO: describe how type equivalence should work

## Expression Language
   Files to change:
   * `UHExp.re` : add line item
   * `ZExp.re` : add line item
### `UHExp.re` :
To support type alias, we need to add a new line item to the language and the proposed syntax is 

`DefLine(TVar.t, UHTyp.t)` for abstract syntax

`define t is type in e`  for GUI syntax

### `ZExp.re` :
By considering the cursor positions of `DefLine`, we need to add two more zline items which are

`DefLineT(TVar.t, ZTyp.t)` (when cursor is on the type)

### Semantics
The static semantics of this expression is that type variable `t` will be classified as `S(type)` in the type variable context. The result is `t` will be definitionally equivalent to `type` in expression `e`.


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

#### Files to change:
* `ExpandingKeyword.re` : add a new keyword 
* `Actionre.re` : add new shape

For the keyboard interactions, we will handle the construt of the `DefLine` the same way as the `LetLine`. As a result, when cursor is at a place where expression is expected, if user type "define" and then hit space, we will expand the line to an `DefLine`.

#### `ExpandingKeyword.re` :
1. For `ExpandingKeyword.t`, we need to add define keyword

        type t = ...
           | Define
2. Update other functions accordingly

#### `Action.re` :
1. For `Action.shape`, we need to add new shape define

        type shape = ...
                   | SDefine

2. Update other functions and utilities.

### Cursor Inspector
TODO: adding new error messages for kind inconsistencies and invalid type variables

### Pretty Printer
TODO: adding support for new constructs
