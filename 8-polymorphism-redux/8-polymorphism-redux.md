# Introduction

TODO

# Design Thoughts

Raw:

* On Polymorphic Gradual Typing's Implicit Polymorphism adds complexity w.r.t. Explicit Polymorphism, so let's stick with explicit for now. Also, Gradual System F (paper) discusses how you can support Implicit Polymorphism dynamically as part of the runtime semantics (unsure if this scales to other implementations besides Gradual System F (language))
* If you want to keep System F syntax, you must sacrifice strong Graduality, however you can keep strong Graduality and support arbitrary System F-style Explicit Polymorphism by adding new syntactic forms.
* Gradual System F (paper)'s Gradual System F (language) notion of Weak Graduality is a reasonable tradeoff on usability -- but this approach doesn't support blame tracking.
* Lambda B supports blame tracking but has even worse Graduality and a weaker Gradual Parametricity problems than Gradual System F (language): Given two related type abstractions and two arbitrary types, we cannot reason about both corresponding type applications. We can only reason about the body of the type abstractions (after removing type applications). Does not track type instantiations that occur on imprecise types.
* Graduality and Parametricity: Together Again for the First Time 's PolyG^v sacrifices System F syntax, and requires explicit upcasts and downcasts, but does meet Graduality. Requires a little more burden for the programmer. Notion of Gradual Parametricity is stronger than Lambda B
* In a sense, if you're okay with Explicit Polymorphism then why not be okay with explicit upcasting and downcasting for sealing? Sealing combinators can help the programmer too.
* Other point in the design space: Eagerness of type constraint checking. Gradual System F (language) is more eager in how it reduces to an error and than PolyG^v.
* Other point in the design space: Tracking type instantiations that occur on imprecise types: Lambda B doesn't; PolyG^v and Gradual System F (language) does.

Directionally leaning towards:

Implement a system similar to PolyG^v, but think carefully about the syntax for introducing binding forms to potentially avoid the awkward outward scoping of type variables. Because we have more expressive power than ASCII or Unicode when displaying Hazel programs, I think we can make the explicit sealing/unsealing less noisy.

