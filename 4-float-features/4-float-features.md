#Introduction

Num type has been separated into Int type and Float type.
The old Num type operators and features only support Int types, so there is a need to support Float types also.

We propose the addition of basic operators to Float types to match those of Int types.
Also, we propose more support in recognizing Int and Float types, such as operator type prediction and type casting.
When adding these features, consistency will be the key.

#Float Operators

##Boolean Float Operators

There should be boolean operators for float types too.
Currently, there are 3 boolean operators, only supporting Int types 
* `<`
* `>`
* `=`

We propose the addition of the same operators, but for Float types.
In order to be consistent with Float Ops for plus, minus, and times, we will **Not** overload the operators.
Instead, the operators will have a following **"."** after them.
* `<.`
* `>.`
* `=.`