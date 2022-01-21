## Overview

The principle of design is to postpone building syntax tree when possible. Since we should transform it into Grain program, we want to make empty hole also like built-in type like `Number`/`Bool`.

Here is an example,
```
import Hazel from "hazel"

let p1 = Hazel.htrue
let p2 = Hazel.hfalse
let p3 = Hazel.buildEmptyHole(312)

let result = Hazel.and(p1, p3)
let result = Hazel.and(result, p3)

Hazel.print_prog(result)
```

The functions like `and()`, `buildEmptyHole()` are all implemented in `Hazel` module.

## Memory

We want to make indeterminate expression end with `0x010` so that functions like `Hazel.add()` can distinguish whether its parameter is boxed value or not.

The constant boolean value is the same as Grain's boolean value. It ends with `0x100`.

The syntax tree is an enum type in Grain. According to Grain's document, it is stored in heap and the value of object is a pointer. To make indeterminate value ending with `0x010`, we manually change the value.
