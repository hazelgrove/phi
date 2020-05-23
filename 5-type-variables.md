# Introduction
Several future Hazel extensions will require support for type variables, 
including PHI 3 (Explicit Polymorphism) and PHI 2 (Type Aliases). This 
PHI adds basic support for type variables to Hazel. Initially all type
variables will be unbound because there are no binding constructs.

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

## Expansion
Expansion from unexpanded types to expanded types is defined by a judgement
of the form `TyVarCtx.t |- UHTyp.t ~> HTyp.t` with the following new rules:

1. `T |- TyVar(NotInVHole, t) ~> TyVar(idx_of(t, T), t)`
2. `T |- TyVar(InVHole(_), t) ~> TyVarHole(fresh(), t)`

For all other unexpanded type forms, expansion proceeds as before. The type
variable context is threaded through inductively.


## Kind System
### Kind Consistency
Kind consistency is defined by a judgement of the form `Kind.t ~ Kind.t` with
the following rules:

1. `k ~ k`
2. `KHole ~ k`
3. `k ~ KHole`

### Kinding
Kinding is defined by bidirectional kinding judgements of the following forms:

  1. Kind Analysis: `TyVarCtx.t |- HTyp.t <= Kind.t` 
  2. Kind Synthesis: `TyVarCtx.t |- HTyp.t => Kind.t`
  
The kinding rules are as follows:

1. `T |- THole => KHole`
2. `T |- Int => Type`
3. `T |- Float => Type`
4. etc. for other base types
5. `T |- Prod([ty1, ..., tyn]) => Type` if 
   a. `T |- ty1 <= Type`
   ...
   b. `T |- tyn <= Type`
6. `T |- List(ty) => Type` if `T |- ty <= Type`
7. `T |- Arrow(ty1, ty2) => Type` if `T |- ty1 <= Type` and `T |- ty2 <= Type`
8. `T |- Sum(ty1, ty2) => Type` if `T |- ty1 <= Type` and `T |- ty2 <= Type`
9. Subsumption: `T |- ty <= k` if `T |- ty => k'` and `k ~ k'`

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