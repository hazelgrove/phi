# Introduction

Num type has been separated into Int type and Float type.
The old Num type operators and features only support Int types, so there is a need to support Float types also.

We propose the addition of basic operators to Float types to match those of Int types.
Also, we propose more support in recognizing Int and Float types, such as operator type prediction.
When adding these features, consistency will be the key.

# Float Operators
*overloading will be avoided until a more general consensus for overloading is set.

## Boolean Float Operators

There should be boolean operators for float types too.
Currently, there are 3 boolean operators, only supporting Int types 
* `<`
* `>`
* `=`

We propose the addition of the same operators, but for Float types.
In order to be consistent with Float Ops for plus, minus, and times, we will **Not** overload the operators*.
Instead, the operators will have a following **"."** after them.
* `<.`
* `>.`
* `=.`

## division operator

There should be a division operator to complete the set of basic arithmetic operators.
There should be one operator `/` for Integer division and another for float division `/.` to avoid overloading*.
Also, casting will be avoided for now.

Integer division `/` should take two integer types and return the floor of the result.
    ```
    Ex.
    2 / 2 -> 1
    5 / 2 -> 2
    ``` 

Float division `/.` should take two float types and return a float type based on the result.
    ```
    Ex.
    2. /. 2. -> 1.
    5. /. 2. -> 2.5
    '''
Float division could lead to edge cases such as:
    '''
    1.0 /. 0.0
    -1.0 /. 0.0
    0.0 /. 0.0
    '''
Due to this, we suggest introducing three new constants, following the IEEE 754 format like Ocaml:
* `NaN`
* `Inf`
* `Neg_Inf`
These constants can now be used to evaluate the previous expressions, respectively.

## type prediction
Currently, the user has to manually input a float type operator if they wish to perform some operation on two float types.
However, we suggest a type prediction system for operators where the user's input for an operator will be converted match the context.
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

