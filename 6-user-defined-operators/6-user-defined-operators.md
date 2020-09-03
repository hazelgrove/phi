
[TODO: type equivalence, type variable, cursor inspector and pretty printer]: # 

# Motivation

**User-defined infix operators** provide programmers a way of creating custom functions that can be invoked as *infix* operators.   

# Overview
New operators will have the following syntax: `/([%&*+-./:;<=>?@^|~])+/`, and the expression aliased by the operator will be a 2-ary function type. For example:
```
let ++ = lambda x, y.x + y + 1
# 2 ++ 2 = 5 
```

The associativity and precedence of the operator will be determined by the first operator in the symbolic definition of the user-defined operator. The above example would have left associativity, and precedence equal to `+`. 

I am stealing this design choice from Ocaml ([explained in this blog](https://haifengl.wordpress.com/2014/07/02/ocaml-functions/)).

# Proposed Changes
## Operator Variables
To begin support for user defined operators, we need to be able to use the `LetLine` constructor with operator symbols as the pattern. 

We might do this by updating `Var.re` as follows:
- Change the `valid_regex` string to `"^([_a-zA-Z][_a-zA-Z0-9']*)$|^([%&*+-./:;<=>?@^|~]+)$"`, so variables can be either alphanumeric, or composed exclusively of operators.
- Add function `is_operator` which checks against the second half of the above regex. 

## Edit Actions/UI changes
Next we'll need a way of creating edit states, where the operator specified is something like `<+>`.

TODO: Design how to add operators as functions in context window (UI change).

TODO: Design key stroke to enter previously defined custom operators. 

## Expression Operators
TODO: Design how to place user defined operators into `seq` data structure, and how to modify precedence/associativity lookup functions in `Operators_Exp.re`. 