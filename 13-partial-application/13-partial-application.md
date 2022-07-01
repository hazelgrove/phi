## Overview

This is a proposal to for partial application of functions that take multiple inputs in the form of tuples rather than currying.

## Motivation

Currying is likely the most common way to construct a function that can take multiple inputs in functional programming languages. It has the advantage that the function can be partially applied, meaning that the inputs don't all have to be applied at once. This method does however have the limitation that the inputs must be applied in order. You cannot partially apply a function to just the second input. An alternative way to take multiple inputs is to take these inputs in the form of a tuple, but this strategy currently doesn't support partial application altogether.

This is a proposal for a launguage feature that will allow functions that take multiple inputs in the form of a tuple to be partially applied. Instances of this method will not have limited ordering like currying does due to the fact that all elements of a tuple are treated equally.

## Implementation

The method of partial application described here is already possible, but can be unnecessarily cumbersome to implement in practice. It involves constructing a new function literal that takes the unapplied arguments as inputs, then puts them where they should go in the application of the original function. Below is an example of how the function `f` can be partially applied to just its second argument, `y`.

```
λ(a, c).{f (a, y, c)}
```

The language feature described in this proposal is nothing more than the strategy applied above wrapped up in a sugar that makes it simpler and cleaner to write in practice. 

It will work by introducing a deferral expression: `~`, which is an external expression. If a function that takes an n-tuple as input is applied to an n-tuple, some of the expressions in the tuple can be replaced with deferrals. This indicates that that argument is being skipped and will be applied later. The partial application will evaluate to a new function that takes in the skipped arguments and outputs the result of the original function applied to *all* arguments.

With this, the above example can be simplified to:

```
f (~, y, ~)
```

The deferral cannot synthesize to or analyze against any type. This is fine, as the only place it can exist (without being wrapped in an expression hole) is in an n-tuple literal that is being applied to a function that takes an n-tuple as input.

During elaboration, the partial application is replaced with a new function literal that takes in the unapplied arguments, then inserts them where they belong in the application of the original function. This process is the expansion of the sugar that was described in the first paragraph. Since this is all just sugar, the internal expression language doesn't have to be modified at all.

### Examples

```
val f: ('a, 'b, 'c) → 'd
val x: 'a
val y: 'b
val z: 'c
```
|External Expression|Internal Expression|Synthesized Type|
|:--|:--|:--|
|`f (x, y, z)`|`f (x, y, z)`|`'d`|
|`f (x, ~, z)`|`λb.{f (x, b, z)}`|`'b → 'd`|
|`f (~, y, ~)`|`λ(a, c).{f (a, y, c)}`|`('a, 'c) → 'd`|
|`f (~, ~, ~)`|`λ(a, b, c).{f (a, b, c)}`|`('a, 'b, 'c) → 'd`|
