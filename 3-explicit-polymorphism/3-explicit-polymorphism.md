# Introduction

Currently in Hazel every function has to be assigned a monomorphic type.
This makes it impossible to write code that operates on more than one type
of data. We currently have an example that implements `map : (Num -> Num)
-> [Num] -> [Num]`. By adopting this proposal, Hazel's map function can
have the type `forall a. forall b. (a -> b) -> [a] -> [b]`, and thereby be useful for
a larger set of values.

Specifically, we propose extending Hazel with two type constructs
and two expression constructs, bringing Hazel's type system up to
a System F with holes than a Simply Typed Lambda Calculus with holes.

# Type Language

We'd have to extend UHTyp.t with `TyVar` and `Forall` cases:

  ```reason
  /* UHTyp.re */
  
  type operand =
  ...
  | Forall(TPat.t, t);
  | TyVar(VarErrStatus.t, Var.t)
  ```

  A type pattern is the part of the language between the `forall` and the `.`
  in `forall a. <type>`. Type patterns will be very simple to begin with:

  ```reason
  /* TPat.t */
  type t =
  | Hole(MetaVar.t)
  | Var(Var.t)
  ```

  <!-- TODO: Talk about how TPat.t's might expand in the future -->

# Expression Language

  ```reason
  /* UHExp.t */
  type operand =
  ...
  | TyLam(TPat.t, block)
  | TyArg(UHTyp.t)
  ```

# Construct actions

  We'll be adding the following to `Construct(shape)`:

  ```reason
  type shape =
  ...
  | SForall
  | STyArg
  ```

  `Construct(SForall)` will be triggered when the user types "forall" followed by
  a space in a type hole. This will add a Forall type with the cursor at the type
  pattern hole and a hole as the body.

  `Construct(STyArg)` will be triggered when the user types "type" followed by a
  space in an expression hole. This will add a `STyArg.t` in the expression with a
  type pattern hole.

# Concrete Syntax / Representation

  Forall's appear within types and look like: `forall a. ?`, where `a` is
  a TyVar.t.

  Type vars in types appear as a string of lowercase letter without any prefix
  or constructor.

  Type Vars in expressions appear as `(type a)` where `a` is also a string of
  lowercase letters.

  Type Lambdas are written like a lambda: you start with `\`. Then you write
  "type" followed by a space which will construct a type lambda. If however, you
  type `type<space>` into an existing lambda that has a non-empty type
  annotation, the pattern of the lambda will be put into an error state until
  either you delete the space following "type" or the type annotation. The final
  representation is `λ a:type.{ }`.

# Backspace actions

  What happens when you press backspace at the following positions? (`|`
  represents the cursor.) I don't fully understand how this works visual
  indicators. Any backspace that does not change the tree or the cursor
  but only changes the visual indicator is not taken into account here.

## Forall

  * `forall| a. body` or `forall a.| body`

  Either of these remove the `forall a.` prefix and leave just the body.

  * `forall a|. type

  If the type variable is a single character, replace the type variable with a TyPat.Hole,
  otherwise remove the last character of the type variable.

  * `forall a. type|`

  If the body is not a type hole, this forwards the backspace action onto the
  body, leaving the forall untouched. However, if the body is a type hole, this
  removes the whole type, leaving only a type hole.

## Type Vars

  * `(type| a)` or `(type a)|`

  The entire expression is replaced with a hole.

  * `(type |a)`

  The cursor is moved to `(type| a)`.

  * `(type a|)`

  If the type variable is a single character, replace the type variable with a TyPat.Hole,
  otherwise remove the last character of the type variable.

## Type Lambdas

  * `λ |a:type. { body }` or `λ ?|:type. { body }` or `λ a:type. { |body }` or `λ a:type. { body }|`

  
  The type lambda is removed, leaving just `|body`

  * `λ a|:type. { body }`

  One character is removed from the type variable "a". If it's only one char,
  we're left with `λ ?|:type. { body }`

  * `λ a:|type. { body }`

  The cursor is moved back to `λ a|:type. { body }`.

  * `λ a:type|. { body }`

  The type lambda is converted to the normal lambda `λ a:typ|. { body }`.
  Similarly, typing "d" at this cursor position should render a normal lambda
  with the type annotation "typed".

  * `λ a:type. { body| }`

  Any backspaces here are forwarded the body unless the body is a hole, in
  which case, backspace deletes the whole expression, leaving just a hole.

# Movement actions

  The semantics of MoveRight and MoveLeft should be clear based on the cursor
  positions mentioned in the "Backspace actions" section above.

  <!-- TODO: Talk about what happens with contexts / where we'll need them -->
