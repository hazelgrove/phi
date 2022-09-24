# Grift: Toward Efficient Gradual Typing for Structural Types via Coercions

Author: Andre Kuhlenschmidt, Deyaaeldeen Almahallawi, Jeremy G. Siek
Score /5: ⭐️⭐️⭐️⭐️⭐️
Status: Reading
Type: Paper

### Notes:

- type-based casts will add one proxy to cast chain on each recursive call, thus $O(n)$ catastrophic extra time overhead(See quick sort example, $O(n^2) → O(n^3)$)
- on the contrary, coercions compress the casts which may incur extra time cost on each step(**when and how exactly?**), but compressed casts will be constant later
- **the core design innovation:** what coercions do on function types is to merge proxies to achieve space efficiency.

```jsx
// pseudo code for proxy-merging logic:
if (TAG(v: obj) is not proxy)
	build_proxy(v: obj, c: crcn);
else {
	c2: crcn = compose(proxy→crcn, crcn c);
	build_proxy(p->wrapper, p->closure, c2);
}
```

- no general-purpose global optimizations in Grift such as constant folding etc. These misc optimzations are done after compile to c, and by clang.
- a local optimiza: eliminate useless casts in untyped code region by not allocating proxy for it.
- data representation:
    - $\text{Int/Bool/Char}$: stored as is in **64-bits**
    - $\text{Function}$: pointer to either a flat closure or a **proxy closure**, indicated by the lowest bit of pointer
        - flat: omitted here
        - proxy: for functions that **has been cast**, consists of
            1. ptr to a wrapper function
            2. ptr to original closure
            3. ptr to a coercion(**what**)
    - $\text{Dyn}$: 64-bits integer, with 3 lowest bits indicating the type that has been **injected**, and 61-bits that is either inlined value(for Int/Bool) or ptr to a pair of 64-bits chunks of one value and one type.
    - $\text{Coercion}$: 64-bits, lowest 3-bits indicate the type of coercion(projection, injection, sequence, etc.) The 61-bits store a ptr to structures as below: (except: identity coercion → leave them unused)
        - $T?^p$($\text{Dyn} → T$): 2 x 64 bits, one for a ptr to $T$ and the second is a ptr to the blame label(quoted String)
        - $T!$($T → \text{Dyn}$): 64-bits, ptr to $T$
        - $c_1, ..., c_n -> d$: (n + 2) x 64 bits, 1st stores the second tag, 2nd stores coercion on return, the rest n blocks each store a coercion for one argument.

### Questions:

- how is coercions done to accompolish $O(n)$ space complexity, and why $size(c) ≤ 5*(2^h - 1)$, $c$ is coercion, $h$ is the height of a coercion(how is it defined?)

> the height of a coercion is bounded by the height of the types in its source program
> 
- what is formal definition of “proxy”? how is it constructed?
- what is the definition of “normal form” of coercions?
- for (mutable) reference coercion, why apply c when reading and d when writing?
- what does `UNTAG(obj v)` exactly do in the applying function `coerce(obj v, crcn c)`?
- what does the following point means:

> Because the coercion implementation distinguishes between regular closures and proxy closures, one might expect closure call sites to branch on the type of closure being applied. However, this is not the case because Grift ensures that the memory layout of a proxy closure is compatible with the regular closure calling convention.