# Introduction

<!-- TODO: what are labeled products -->
Labeled products are a value similar to products, but some/all elements have a label.  This label can be used then for projection, so elements within the product can be accessed either positionally or by the label.  
<!-- TODO: why do we want them in Hazel -->
Adding labeled products helps make the tuples become more robust, so more complex products can be easily used.  Accesing a longer tuple using only positional arguments adds unneccsary bulk to the code.  Having labels allows for easy projection.  The labeled products can also serve a similar use as records in other languages.
<!-- TODO: what do we have now -->
Currently only unlabeled tuples are supported in Hazel, and elements can only be accessed positionally using let statements or case statements.
# Labeled Product Types

TODO: add `.label` as a new type form <br/>

&tau; ::= ... <br/>
| .label <br/>
| .label1 &tau;1, .label2 &tau;2 ...  , .labeln &tau;n <br/>

<!-- TODO: do we want to allow partially labeled product types? -->
Partially labeled product types are allowed, and labels and non-labeled positions can be interleaved.
For example, the labeled product type `(.x Num, Num, .y Num)` is allowed.  

Bcause a labeld product may still be used with a positional arguments with partially labeld types, the type equivalence considers the order of the labels with equlivalence.  For example, `(.x Num, .y Num, .z Num) != (.z Num, .y Num, .x Num)` because the order of the labels is different.

Singleton label products are supported.  For example, `.x Num` is supported.

## Type Syntax Errors

A label must be followed by a valid type and comma operator, not another label.  For example, `.label1 .label2 ty` produces an error.<br/>
(Question: Would this produce an error given that singleton products are supported?)

A label cannot exist by itself, it is given meaning by having a type follow it.  For example, `.label1 ` by itself produces an error. <br/>
Proposed Error Message: Error Curosr appears on `.label1`<br/>
Expecting a type of Labeled Product / Got Inconsitent type of Label

Elements in the tuples need to be separated by commas, if they are not then this produces an error.  For example, `.label1 ty .label2` produces an error. <br/>
Proposed Error Message: Error cursor appears on `.label2`<br/>
Expecting a type of Labeled Product / Got Inconsitent type of Label

Duplicate labels within a tuple are not valid, so they produce an error.  The error will appear on the subsequent duplicate uses, not on the type as a whole.  FOr example, the type `.label1 Num, .label1 Num, .label2 Num, .label1 Bool` will have erros on the second use of  `.label1 Num` and  `.label1 Bool`

  <!-- - allow non-labled prefix, but once you use a label as subsequent positions have to be labeled
    - alternatively, allow labeled and non-labeled positions to be interleaved
    TODO: how does this affect type equivalence? e.g. are `(.x Num, .y Num, .z Num) == (.z Num, .y Num, .x Num)`
    TODO: singleton labeled products -- should we support them (`.x Num` is a labeled product type?)
    TODO: syntax errors
     * `.label1 .label2 ty`
      * `.label1` by itself
       * `.label1 ty .label2`
        * we will need some way to mark erroneous uses of labels and indicate that in the cursor inspector
	 * duplicate labels: `(.label1 Num, .label1 Num)` is not a valid type, so we also need duplicate label errors
	   - does the error message go on the subsequent uses, or on the type as a whole? -->

# Labeled Tuple Expressions
TODO: add `.label` as a new expression form
TODO: similar considerations as above
TODO: can you omit labels by providing values in order: `(1, 2, 3) <= (.x Num, .y Num, .z Num)`
TODO: "Record punning" in Reason: `{x, y, z} => {x: x, y: y, z: z}` -- is there anything analogous that we can do? Does this interact with positional values? `(y, x, z) <= (.x Num, .y Num, .z Num)` does that operate positionally or via punning?
TODO: partially labeled values, where some of the arguments are in order: `(1, 2, .z 3) <= (.x Num, .y Num, .z Num)`. what about interleaving vs. requiring all the explicit labels at the end ala Python?
TODO: what are the type synthesis and type analysis rules for the labeled tuple expressions

# Projection Expressions
TODO: add `e.label` as a new expression form
TODO: can you press backspace on `e |.label` and get to `e.label`?
TODO: can you press space on `e|.label` and get to `e .label`
TODO: what are the type synthesis and type analysis rules?

# Labeled Tuple Patterns
TODO: similar considerations to labeled tuple expressions
TODO: we might want punning for labeled tuple patterns:
```let f = fun(.x, .y) ...
f(.x 1, .y 2)```
instead of
```let f = fun(.x x, .y y) ...
f(.x 1, .y 2)```
TODO: what are the pattern type synthesis and pattern type analysis rules? (ask Yongwei for paper draft if you want to formalize)

# Other Ideas

TODO: clean up

# Overall Possibilities

Option 1: Use reason like ~label annotations
Option 2: Use a space to separate between label and type/expression/pattern.  Period operator is added before label to specify that it is a label. Ties in well with linking the period to labeled products, as the period is also used for projection.  Possible problem: Issues with using a period as both binary operator (projection) and unary operator (label definition)
Option 3: Use space operator between labels and type/expression/pattern.  Problem: No way to tell distinction between undefined function variable application and labeled expression
Option 4: Use colon operator between labels and type/expression/pattern.  Problem: Confusion between type annotations and labeled pair type annotation
Option 5: Use colons and braces to signify a labeled tuple.  Problem:  This syntax is more typical for a record, and may create confusion about labeled tuples as a value and records as a type definition, as well as create issues if there are any future plans for adding records to Hazel, makes development task larger to define braces syntax

# Labeled Product Types

Currently: `ty1, ty2, ..., tyn`
With Labels: `~label1: ty1, ~label2: ty2,..., ~labeln: tyn`
With labels: `.label1 ty1, .label2 ty2, ..., .labeln tyn`
With labels: `label1 ty1, label2 ty2, ..., labeln tyn`
With Labels: `label1: ty1, label2: ty2, ..., labeln: tyn`
With labels: `{label1: ty1, label2:ty2, ..., labeln:tyn}'

# Labeled Product Expressions

Currently: `e1, e2, ..., en`
With labels: `~label1=ty1, ~label2=ty2, ..., ~labeln=tyn`
With Labels: `.label1 e1, .label2 e2, ..., .labeln en`
With Labels: `label1 e1, label2: e2, ..., labeln: en`
With Labels: `label1: e1, label2: e2, ..., labeln: en`
With labels: `{label1: e1, label2:e2, ..., labeln:e2}'

# Labeled Product Patterns
Currently: `p1, p2, ..., pn`
With Labels: `~label=ty1, ~label=ty2,..., ~labeln=tyn`
With Labels: `.label1 p1, .label2 p2, ..., .labeln pn`
With Labels: `label1 p1, label2 p2, ..., labeln pn`
With Labels: `label1: p1, label2: p2, ..., labeln: pn`
With labels: `{label1: p1, label2: p2, ..., labeln: pn}'

In pattern position, we would also want to support type annotations.

# Projection

Currently: no projection
With Labels: `e.label`

(For many of these, we will need label holes.)
