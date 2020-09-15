
[TODO: type equivalence, type variable, cursor inspector and pretty printer]: # 

# Motivation

**User-defined infix operators** provide programmers a way of creating custom functions that can be invoked as *infix* operators.   

# Overview
New operators will have the following syntax: `/([_][%&*+-./:;<=>?@^|~])+[_]/`, and the expression aliased by the operator will be a 2-ary function type. For example:
```
let _++_ = lambda x, y.x + y + 1
# 2 ++ 2 = 5 
```

The associativity and precedence of the operator will be determined by the first operator in the symbolic definition of the user-defined operator. The above example would have left associativity, and precedence equal to `+`. 

I am stealing this design choice from Ocaml ([explained in this blog](https://haifengl.wordpress.com/2014/07/02/ocaml-functions/)).

# Proposed Changes
## Edit Actions Semantics
First we need to be able to type patterns containing operators into LetLine expressions.

### Patterns
`Action_Pat.re` - add cases to `ana_perform_operand` to allow creating operator symbols. For example,
```
| (Construct(SOp(os)), CursorP(OnDelim(_, side), Wild(_)))
    let index =
      switch (side) {
      | Before => 0
      | After => 1
      };
    let operator_char = Action_common.shape_to_string(SOp(os));
    ana_insert_text(ctx, u_gen, (index, operator_char), "_", ty);
```
to add operators directly after a wild `_`.

Will also need to add cases for injecting into patterns like '_<>_', '_<>*_', and '_*<>_'.

### Expressions
`Action_Exp.re` - update cases when cursor is on top of operator. 

Specifically in this case
```
| (Construct(SOp(os)), ZOperand(zoperand, surround))
```
we want to stack symbols into a single operator, then try to resolve the operator. 

For example, `_ *<insert * here> _` should result in `_ ** _` as opposed to `_ * _ * _`.

## External Language Syntax
`Var.re` - add an operator regex to accept variables of the form `/([%&*+-./:;<=>?@^|~])+/`.

## Type Checking


## Expression Operators
TODO: Design how to place user defined operators into `seq` data structure, and how to modify precedence/associativity lookup functions in `Operators_Exp.re`. 
