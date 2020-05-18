# Introduction

`Num` type has been separated into `Int` type and `Float` type.\
The old `Num` type operators and features only support `Int` types, so there is a need to support `Float` types also.

We propose the addition of basic operators to `Float` types to match those of `Int` types.\
Also, we propose more support in recognizing `Int` and `Float` types, such as operator type prediction.\
Finally, we suggest a braoder set of valid `Int` and `Float` inputs.
When adding these features, consistency will be the key.

# Float Operators
*overloading will be avoided until a more general consensus for overloading is set.

## Boolean Float Operators

There should be boolean operators for `Float` types too.\
Currently, there are 3 boolean operators, only supporting `Int` types.
* `<`
* `>`
* `=`

We propose the addition of the same operators, but for `Float` types.\
In order to be consistent with `Float` operators for plus, minus, and times, we will **Not** overload the operators*.\
Instead, the operators will have a following **"."** after them.
* `<.`
* `>.`
* `=.`

## Division Operator

There should be a division operator to complete the set of basic arithmetic operators.\
There should be one operator `/` for Integer division and another for Float division `/.` to avoid overloading*.\
Also, casting will be avoided for now.

Integer division `/` should take two arguments of `Int` types and return a `Int` type, the floor of the result.
   
    ```
    Ex.
    2 / 2 -> 1
    5 / 2 -> 2
    ``` 

Float division `/.` should take two arguments of `Float` types and return a `Float` type, the result.
    ```
    Ex.
    2. /. 2. -> 1.
    5. /. 2. -> 2.5
    ```

`Float` division could lead to edge cases such as:
    ```
    1.0 /. 0.0
    -1.0 /. 0.0
    0.0 /. 0.0
    ```
Due to this, we suggest introducing three new constants, following the IEEE 754 format like Ocaml:
* `NaN`
* `Inf`

These constants can now be used to evaluate the previous expressions, respectively.

## type prediction
Currently, the user has to manually input a `Float` type operator if they wish to perform some operation on two `Float` types.\
However, we suggest a type prediction system for operators where the user's input for an operator will be converted match the context.\
This idea can later be expanded to other types as well.
* Current behavior
    ```
    2. | -> 2. +|
    2. | 3. -> 2. +| 3.
    3 | -> 3 +|
    ```
* New behavior
    ```
    2. | -> 2. +.|
    2. | 3. -> 2. +.| 3.
    3 | -> 3 +|
    ```

In predicting the operator type, we will use a three step rule.\
"Type of interest" will denote the types of the arguments that is being considered to determine the appropriate operator to construct given an input. 
1. If the editted expression is in an analytical position that is either `Int` or `Float`, the operator of the corresponding type will be used.
2. If the expression is in synthetic position or the expected type is not explicitly specified as `Int` or `Float`, then the types of the operator's arguments will be inspected. If at least one of the arguments has the type `Int` or `Float`, the corresponding operator will be constructed. In the case of the arguments having conflicting types of interest (`Int` and `Float` in this case), the type of the left argument will be used.
3. In the case that neither arguments of the operator is of the type of interest, the "default" operator will be used, which will be the `Int` operator in the case of `Int` and `Float` operators.

Examples
```
let i:Int = 2. | 3. in ... -> let i:Int = 2. + 3. in ...
2. | 3 -> 2. +. 3
3 | 2. -> 3 + 2.
true | true -> true + true
```

# Valid Int and Float Inputs

## Thousands separator
Ocaml supports `_` in integer and float literals as a way to better visualize long numbers.\
Underscores are ignored by Ocaml during evaluation, but there are some inconsistencies such as `_1` being an invalid integer while returning `1.` when the string is passed to `float_of_string`.

Hazel is currently using `[int|float]_of_string_opt` function provided by Ocaml to convert user inputs to `Int` or `Float` types, but it will eventually be replaced by custom functions to expand or restrict the possible inputs for `Int` and `Float` types. For now, a helper function should be added to treat number literals with leading underscores as a `Var` type instead of `Float` type. In other words, `_1` will be considered as a variable in Hazel.

Eventually, we should not accept underscores in number literals. Instead, we should automatically include `,` in number literals to separate the digits. For instance, `1234567` will now be represented as `1,234,567`. The location of the separator will be automatically be adjusted as the user edits the number literal and will be taken care of by the prettyfier logic.\
The only loss in feature from this change will be that there will be no way to separate the decimal digits in float literals such as `1.233_534`. However, this loss does not appear to be very significant at this time. In the case that the user needs to know the significance of a digit, we could later introduce a feature that takes the position of the cursor in a number literal and return the significance of the digit.
