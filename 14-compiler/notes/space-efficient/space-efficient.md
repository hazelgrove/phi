# Space-Efficient Gradual Typing

Author: David Herman

### Main Contribution

- the paper defines the coercion language and its typing rules.
- they implement their set of typing and evaluation rules in the work which elaborates source program(GLTC) into a target language with **explicit type casts**(that is, an expression can be placed anywhere with consistent type) and casts are represented with **coercions** instead of proxy-based implementations.
- they derive the proof of its space-consumption bounds.
- with additional coercion normalization rules(on Fail corercion), this implementation can reveal ill-typing bugs on function’s definition site instead of on its application.

### Takeaways

![Untitled](assets/untitled-00.png)

![Untitled](assets/untitled-01.png)

![Untitled](assets/untitled-02.png)

![Untitled](assets/untitled-03.png)

![Untitled](assets/untitled-04.png)
