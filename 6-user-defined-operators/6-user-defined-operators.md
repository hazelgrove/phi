
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
## External Language Syntax
First we will need a way to store and represent user defined operators in seqs. 

`Var.re` - add an operator regex to accept variables of the form `/([%&*+-./:;<=>?@^|~])+/`.


`Operators_Exp.re`

 - add a `UserOp(string)` variant to the `Operators_Exp` type. 
 - to_string should just return the string held in the variant
 - precedence/associativity functions will take first character of the string in the variant, then look up that precedence/associativity.

## Edit Actions Semantics
Next, we will need to be able to type patterns containing operators into LetLine expressions.

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

Will also need to add cases for injecting into patterns like '\_<>\_', '\_<>*\_', and '\_*<>\_'.

### Expressions
`Action_Exp.re` - update cases when cursor is on top of operator. 

Specifically in this case in `syn_perform_opseq`
```
| (Construct(_), ZOperator(zoperator, _)) =>
```
we no longer want to move the cursor, then construct a new operator. Instead, this case should change the current zoperator from one of the predefined operators into a `UserOp(string)`, to be described in the external language syntax discussion.

For example, `_ *<insert * here> _` should result in `_ ** _` as opposed to `_ * _ * _`.

### Cursor Info
`CursorInfo_Exp.re` - add case to recurse towards the cursor in `syn_cursor_info_skel` by matching on the case
```
| BinOp(_, UserOp(op), skel1, skel2) =>
```
We should resolve the type of the operator, and the lhs/rhs operands should analyze against the input types of the operator's type. 

## Internal Language Syntax
`DHExp.re` - add a new operator type `BinUserOp`, so we can have varying types on the left/right side of the operator.

## Type Checking 
`Statics_Exp.re`  - Add a case to the function `syn` to match the `UserOp` type, synthesize the type of the left and right skels by resolving the operator, then `ana_skel`. 
- Add a case to the function `syn_fix_holes_skel` to match the `UserOp` type, synthesize the type of the left and right skels by resolving the operator, then `ana_fix_holes_skel`. 
 - Similarly, add a case to the function `syn_nth_type_mode`, synthesize types of lhs and rhs, then `ana_go` with correct types. 

## Evaluation
`Evaluator.re` - Evaluation should be handled by substitution at the level of a let-expression. Free user-defined operator symbols result in an indeterminate form. 

For user-defined operator *op*, 2-ary function *f*, and expressions e_1, e_2, define substition into a user-defined operator as follows:

![equation](https://latex.codecogs.com/gif.latex?%5Bf%20%5C%20/%20%5C%20op%5D%28e_1%20%5C%20op%20%5C%20e_2%29%5Cequiv%20%28f%20%5C%20%28%5Bf%20%5C%20/%20%5C%20op%5De_1%29%20%5C%20%28%5Bf%20%5C%20/%20%5C%20op%5De_2%29%29)


