# Introduction

Several future Hazel extensions will require support for type variables, a
prerequisite for type aliases including PHI 3 (8?) (Explicit Polymorphism).
This PHI adds support for type variables and type aliases to Hazel.

# Type Language

## Type Identifiers

We define a type `TyId.t` as follows:

```
type TyId.t = string
```

The syntax of type variable identifiers is initially the same as expression
variable identifiers, but this may change in the future. We restrict type
variable identifiers from using keywords in the same way (to support 
extraction).

## Type Pattern

Type patterns are the part of the language between `type` and `=` in `type a = <type> in e`

It is represented as greek letter `rho` in the attached
[latex document](./latex/kind-judgements.pdf).

```
type TPat.t =
  | EmptyHole(MetaVar.t)
  | TyVar(VarErrStatus.t, TyId.t)
```

## Unexpanded Types

We extend the syntax of `UHTyp.t` as follows:

```
type UHTyp.t = ... | TyVar(VarErrStatus.t, TyId.t)
```

## Expanded Types

In expanded types, we will use a de Bruijn indexed representation. This requires
defining a type abbreviation `HTyp.idx` as follows:

```
type HTyp.idx = int
```

Then, we extend the syntax of `HTyp.t` as follows:

```
type HTyp.t = ... 
            | TyVar(idx, TyId.t) 
            | TyVarHole(MetaVar.t, Var.t)
```

The `TyVar` constructor represents a bound type variable. We retain the original
identifier for the purposes of supporting contraction back to a `UHTyp.t`.

The `TyVarHole` constructor represents a free type variable, which is understood
as a type variable hole. Like other holes, each type variable hole has a 
metavariable.

## Kinds

We will need kinds to distinguish types from type-level holes, such as `TyVarHole`.
We define a new type, `Kind.t`, as follows:
```
type Kind.t = KHole
            | Type
            | Singleton(HTyp.t)
```

## Unexpanded Expressions

To support type aliases, we need to add a new line item to the language and the proposed syntax is

`TyAliasLine(TPat.t, UHTyp.t)` for abstract syntax

`type t = <type> in e`  for GUI syntax

### `ZExp.re` :

By considering the cursor positions of `TyAlias`, we need to add two more zline items which are

`TyAliasLineT(TPat.t, ZTyp.t)` (when cursor is on the type)

`TyAliasLineP(ZTPat.t, UHTyp.t)` (when cursor is on the type pattern)


## Expanded Expressions

So that we can still assign a type properly to the resulting `DExp.t` we add the following new syntax:

`TyBindings(TyVarCtx.t, DHExp.t)` for abstract syntax

(TODO: is it ever necessary to display this to the user? If so -- maybe something
like `type bindings [type t = \tau, type x] in e`)

# Static Semantics

This PHI introduces a gradual kind system supporting singletons into Hazel.

The addition of type variables demands we have a type variable context to store them.

We can define bidirectional type elaboration judgements which will replace the existing `UHTyp.expand` function.

Type aliases are given meaning with singleton kinds. A singleton kind is kind dependent on a type.

Singleton kinds to the language demand a subkinding relation and in the context of gradual typing, subkinding is replaced with consistent subkinding relation derived from PFPL chapter 43 and the [Consistent Subtyping For All](https://i.cs.hku.hk/~bruno/papers/gradual-esop18.pdf) paper. In addition to the subkinding relation, we also add the following mutually recursive auxilliary judgements in order to properly assign singleton kinds in the type system: Kind Formation, Kind Equivalence, Kind Assignment, Kind Constructor Equivalence, (along with Consistent Subkinding).

We extend bidirectional expression elaboration to support the new type alias form using these auxilliary judgements, and add a new type pattern analysis judgement that yields new bindings.

## Type Variable Contexts

Type variable contexts are values of type `TyVarCtx.t`, which is defined
as follows:

```
type TyVarCtx.t = list((TyId.t, Kind.t))
```

The position within a type variable context determines the de Bruijn index
(most recent bindings at the head). For this reason, type identifiers *can* 
be duplicated.

## Consistent Subkinding

Consistent Subkinding is defined by a judgement of the form `Kind.t <~ Kind.t`
with the rules presented in the attached
[latex document](./latex/kind-judgements.pdf).

This judgement handles both consistency and subkinding at the same time.

## Kind Formation

Kind Formation is defined by a judgement of the form
`Delta.t; TyVarCtx.t |- Kind.t kind` there is a single rule that says if
`HTyp.t : kind Ty` then we can form the kind `Singleton(Kind.t)`. It is
presented in the attached [latex document](./latex/kind-judgements.pdf).

## Kind Equivalence

Kind Equivalence is a reflexive, symmetric, and transitive relation that caries
kind constructor equivalence through to singleton kinds.
`Delta.t; TyVarCtx.t |- Kind.t_1 === Kind.t_2`  It is presented in the attached
[latex document](./latex/kind-judgements.pdf).

## Kind Assignment

Kind assignment is defined by a judgement
`Delta.t ; TyVarCtx.t |- HTyp.t : Kind.t` with rules presented in the attached
[latex document](./latex/kind-judgements.pdf).

## Kind Constructor Equivalence

Currently, Kind Constructor Equivalence is exactly type equivalence because
Hazel (even after applying this proposal) doesn't support higher kinds. Kind
Constructor Equivalence is a reflexive, symmetric, and transitive relation
where the "type aliasing" using the singleton kind occurs. The judgement
`Delta.t; TyVarCtx.t |- HTyp.t_1 === HTyp.t_2` is defined in the attached
[latex document](./latex/kind-judgements.pdf).

## Type Elaboration

Expansion from unexpanded types to expanded types is defined by
bidirectional judgements `TyVarCtx.t |- UHTyp.t => Kind.t ~> HTyp.t -| Delta`
and `TyVarCtx.t |- UHtyp.t <= Kind.t_1 ~> HTyp.t : Kind.t_2` for consistent
`Kind.t_1` and `Kind.t_2`. These judgements are shown in the attached
[latex document](./latex/kind-judgements.pdf).

`Delta.t`'s `hole_sort` will be changed to a `Hole.t` so we can track type
holes there too. `Hole.t` is an ADT that captures type holes' different `Kind.t`
and `TyVarCtx.t` instead of `HTyp.t` and `VarCtx.t` and we'll still have the
same keyspace (the `u`s are consistent and incrementing across all the holes):

```reasonml
module Hole = {
type t =
  | Expression(HTyp.t, VarCtx.t)
  | Type(Kind.t, TyVarCtx.t)
  | Pattern(HTyp.t, VarCtx.t);

type t = MetaVarMap.t(Hole.t);
```

## Type Pattern Analysis

Type patterns analyze with a type to create new tyvar bindings and hole bindings
`Delta.t_1 ; TyVarCtx.t_1 |- TPat.t <- HTyp.t -| TyVarCtx.t_2 ; Delta.t_2`. This
judgement is in the attached [latex document](./latex/kind-judgements.pdf).

## Expression Elaboration for Type Aliases

The typical expansion from unexpanded expressions to expanded expressions
handles the new type alias case relying on the Type Pattern Analysis judgement
to generate the bindings for the remaining subexpression. See the attached [latex document](./latex/kind-judgements.pdf).

# Action Semantics

## Type Variables

Adding support for type variables requires adding support for character-level 
editing at the type-level. Currently, types are bound to individual uppercase
characters, e.g. `I` inserts the `Int` type immediately. This needs to be 
changed to support natural text editing. We will follow the approach taken 
with variables in expressions and patterns.

We will need to change the action semantics for types to support kinds:

Previously the type-level action semantics was defined by a judgement of
the form:

`ZTyp.t --Action.t--> ZTyp.t`

Now, we will need to define a bidirectional action semantics as with 
expressions by judgements of the following form:

1. `TVarCtx.t |- ZTyp.t => Kind.t --Action.t--> ZTyp.t => Kind.t`
2. `TVarCtx.t |- ZTyp.t --Action.t--> ZTyp.t <= Kind.t`

## Construction of type aliases

TODO: Adapt from polymorphism PHI

## Backspace Actions

TODO: Port from polymorphism PHI
