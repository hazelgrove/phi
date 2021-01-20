**By: Eric Griffis**

## History

| Date        |       Version |
|-------------|---------------|
| Jan ?? 2021 | Initial Draft |

## Introduction

This is a proposal to introduce algebraic data types.

Here is an algebraic data type for Peano-encoded naturals:

```
datatype Nat = Z | S of Nat
```

This `datatype` definition creates a new type `Nat` with two constructors `Z`
and `S`. We want to be able to write `datatype` definitions like this so that we
can determine algorithmically whether `case` expressions on the new type are
both complete and concise.

Here is an inductive definition of the addition of two `Nat`s:

```
let nat_add =
  λm:Int.{
    λn:Int.{
      case m
      | Z => n
      | S m' => S (nat_add m' n)
      end
  }
}
```

The `case` expression is both complete and concise in the sense that one, and
only one, of its patterns will match any given constructor of type `Nat`.

## Illustrative Examples

### Enumerative Types

Bool, Planet

```
datatype Bool = True | False
```

### Composite Types

int option

```
datatype IntOption = IntNone | IntSome of Int
```

### Recursive Types

int list

```
datatype IntList = Null | Cons of (Int, IntList)
```

### Pattern Matching

```
case x
| True => 1
| False => 0
end
```

```
case x
| IntSome x' => x'
| IntNone => 0
end
```

### Exhaustiveness Checks

```
case x
| True => 1
end

(no clause for False ==> compile-time error)
```

### Redundancy Checks

```
case x
| IntSome x => x
| IntSome 1 => 1
end

(second clause will never be used ==> compile-time error)
```

### Holes

Datatype definitions can have holes in them.

```
datatype ? = A
```

```
datatype t = B | ?
```

```
datatype t = C of ?
```

Constructor patterns can have holes in them.

```
case x
| IntSome ? => 1
| IntNone => 0
end
```

A *constructor pattern hole* is a pattern hole in constructor position.

```
case x
| ? x' => x'
| IntNone => 0
end
```

## Design

What are they?

- data types
  - no polymorphism
  - can have holes in definitions and patterns
- constructors
  - are they functions? *no*
    - but are they observationally equivalent to functions?
      - e.g., can they be passed around as values, applied like functions, and type checked?
  - can they be ordered?
- constructed values
  - An application expression with a constructor in function position is a value; It does not beta-reduce.
- patterns

How do they work?

- deconstruction
  - case expressions
  - match failures (no pattern matches the scrutinee) ==> should be impossible w/ exhaustivity checks
- exhaustivity and redundancy checks

- ~"feel and fit" with existing core features~
  - maybe ask Andrew?

- correctness
  - refer to formalism section below for details

- performance considerations?
  - no major concerns
  - not optimizing for performance yet

- naming considerations
  - datatype names are like normal type names
  - constructor names have initial caps
  - name clashes (w/o modules), if we want two constructors in the same scope:
    
```
datatype bool_list = Null | Cons of (Bool, bool_list)

let BoolNull = Null

datatype int_list = Null | Cons of (Int, int_list)
```

(there are issues w/ patterns b/c we don't have pattern abbreviations.)

- API footprint
  - do we have a prelude?

- potential conflicts with existing features
  - variables: name can be initial caps.
    - needs to be changed to only begin with lowercase letters.
  - booleans, lists: should they become ADTs?
    - ideally, they would if we had a syntax extension system for them
  - injections: the generic sum type mechanism in Hazel
    - keep around for pedagogical use
  - assume module names have initial caps

## Formalism

- check Hazelnut POPL papers 

### Syntax

### Static Semantics

POPL 17

### Dynamic Semantics

POPL 19

### Action Semantics

## Implementation

first introduction forms, then elimination forms, then relationships between the two (Gentzen's inversion principle, in logic)

1. add a datatype context and constructor context
  - provide an initial context with a few example types
3. add constructors to expressions
2. add case and pattern matching semantics
4. add datatype definitions

Add a `UHExp` line item for datatype definitions.

Add a keyword `datatype` that expands to a *datatype definition*.

Add a constructor context and a datatype definition term sort with `|` and ` ` as infix operators with ` ` binding more tightly.
Datatype definitions extend the constructor context and the expression variable context.

Change dynamic semantics of ` ` to support datatype constructors.

Add constructor definition holes with a duplicate constructor error flag.

Change pattern matching to support datatype constructors.

Change ` ` operator in pattern from an error to an application.

Add constructor pattern holes that convert to and from normal pattern holes.

- ~must be bundled with core (no module system)~

- any existing implementations?

These are dependencies of this PR (they need to be merged before this):

Yuning's branch is one example of extending the typing context, with type variables.

Yuning and Yongwei's branch `merged-match` might do exhaustivity and redundancy checks.

- time frame

Around 2 person-months? Will make an initial estimate, then provide regular updates in weekly meetings.

- testing plan

Unit + property tests (which properties? any indirectly relevant?)
(if the parser (or David's testing stuff) isn't ready yet, then delete them when I'm done.)
(maybe delay property-based testing until we have synthesis)
(maybe contract-style property tests won't have problem being tied to underlying representation)

- roll-out plan

Will do: dev + test --> all tests pass --> submit PR.
- all at once

Milestones (subject to change, based on blocking tasks):
1. datatype context containing only enums
2. variable names only lowercase
3. pattern matching
4. constructor arguments
5. datatype definitions
6. recursive data types
7. exhaustiveness and redundancy checking

(don't forget to merge dev regularly)

- other contributors
  - wait until the proposal's done, then list anyone who helped.

Yuning, Cyrus, (Andrew, David)

## Counter Examples

- What are they not intended to be used for?
- Examples of what to avoid and why

## Drawbacks

Why should we *not* do this?

- adds complexity to the Hazel core 
  - future Hazel extensions may need to account for ADTs
    - if arrow types are not necessarily functions, then future extensions to the type system will have to account for them.

## Alternatives

- What other possibilities have been examined?
  - structural labeled sums (don't have to be declared)
  - polymorphic variants
    - our constructors are associated with a definition -- they're not purely structural
  - scala-style case classes (we don't have classes)
  - traits? (we don't have objects)
  - open datatypes (e.g., OCaml exceptions)

- What is the impact of not implementing this proposal?

Many future directions depend on them. Hazel is kind of an incomplete language without them.
Some near-term projects that depend on them, or would be improved by them, will be blocked.
  - Hazel Synthesis
  - Hazel Tutor

## References

1. [existing projects][1]
2. [any API documentation][2]
3. [academic research papers or supporting material][3]
4. [alternative libraries / implementations][4]
5. [any related discussion][5]

[1]: https://github.com "GitHub"
[2]: https://hazel.org "Hazel"
[3]: https://hazel.org "Hazel"
[4]: https://github.com "GitHub"
[5]: https://slack.com "Slack"