TODO: 
1. overview - done
2. kind system
3. type equivalence
4. type variable
5. cursor inspector
6. pretty printer

# Motivation

**Type aliases** give users ability to define a shorthand for another type, which is either a user defined type or primitive type. As for benefits, type aliases can reduce the amount of typing as well as increasing the readability of code. 

Moreover, most of popular programming languages provide this feature such as `typedef` in C/C++ and [type synonym](https://wiki.haskell.org/Type_synonym) feature in Haskell. 

# Overview
To be able to define type aliases, first we need to introduce a **kind system** to classify type constructors including the singleton kind which is used to express the type aliases. Second, we need to be able to construct **type variables** to stand for some type.

Moreover, new error messages for cursor inspector is needed. For the pretty printer, we also need to support the new syntax.

# Proposed Changes
## Type-Level Language
### Type Variables

To support type alias, first we need to support type variable in our type level language.

Files to change :
   * `UHTyp.re` : add support for type variable
   * `HTyp.re` :  add support for type variable
   * We need to support type variable context Delta
   * `Action.re` : be able to construct and use type variable 

#### `HTyp.re` :
TODO: propose the change to HTyp.t (see polymorphism branch, Charles Chamberlain)

#### `UHTyp.re` :
TODO: propose the change to UHTyp.t (make sure you consider invalid type variables)

    TODO: writing down the kinding rules for kind inconsistencies and invalid type variables


TODO: how will Action module change

#### `Action.re` :

## Singleton Kind System
File to change:
   * `HTyp.re`

Here we introduce a kind system for Hazel:

    type t ::= ...
    and
    kind k ::= Type
             | Unit
             | Prod(k, k)
             | Arrow(k, k)
             | Sum(k, k)
             | Singleton(t)

Here, we can see the kind and type systems are defined in a mutually recursive manner. 

We use `Type` to classify Bool, Int, Float and Hole (?) and use `Unit` and `Prod` to classify the product type and unit type. Also, we use `Sum` to classify sum type. Finally, we use `Singleton` kind to express type alias for type constructor `t`.
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

## Pattern Language
The pattern language will not change.

## Dynamic Semantics
TODO: does this need any changes?

## User Interface
### Key Bindings

#### Files to change:
* `ExpandingKeyword.re` : add a new keyword 
* `Actionre.re` : add new shape

For the keyboard interactions, we will handle the construct of the `DefLine` the same way as the `LetLine`. As a result, when cursor is at a place where expression is expected, if user type "define" and then hit space, we will expand the line to an `DefLine`.

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
