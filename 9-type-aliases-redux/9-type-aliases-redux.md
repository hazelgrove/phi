# Introduction

Several future Hazel extensions will require support for type variables, a
prerequisite for type aliases including PHI 3 (8?) (Explicit Polymorphism).
This PHI adds support for type variables and type aliases to Hazel.

TODO: Merge with Type-alias content

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
```

# Static Semantics
## Type Variable Contexts
Type variable contexts are values of type `TyVarCtx.t`, which is defined
as follows:

```
type TyVarCtx.t = list((TyId.t, Kind.t))
```

The position within a type variable context determines the de Bruijn index
(most recent bindings at the head). For this reason, type identifiers *can* 
be duplicated.

## Kind Consistency

Kind consistency is defined by a judgement of the form `Kind.t ~ Kind.t` with
the rules presented in the attached
[latex document](./latex/kind-judgements.pdf).

## Type Elaboration

Expansion from unexpanded types to expanded types is defined by
bidirectional judgements `TyVarCtx.t |- UHTyp.t => Kind.t ~> HTyp.t -| Delta`
and `TyVarCtx.t |- UHtyp.t <= Kind.t_1 ~> HTyp.t : Kind.t_2` for consistent
`Kind.t_1` and `Kind.t_2`. These judgements are shown in the attached
[latex document](./latex/kind-judgements.pdf).

# Action Semantics

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
