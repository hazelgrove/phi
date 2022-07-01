# Overview

The Hazelnut calculi are based on the substitution model of evaluation, in which variable bindings are evaluated using an eager substitution pass. This PHI switches evaluation to use the environment model, in which bound variables are added to a run-time environment and lazily looked-up. This increases complexity of evaluation and requires postprocessing to achieve the same result as with substitution, but offers better evaluation performance, memoization of environments in certain tasks (such as hole numbering), and a useful generalized closure construct.

This work was performed for a Master's thesis. The implementation details are described in more detail (including diagrams/formalizations) in Chapters 4 and 5 of [the report][report].

# Motivation

Evaluation-based evaluation offers some general performance advantages over substitution-based evaluation. Firstly, it offers lazy evaluation of variable bindings. This means that substitution passes during evaluation are not required. Also, while substitution is an expression-level operation (and requires a representation of the expression during evaluation), using environments has no such requirement and may allow for easier compilation.

Evaluation using environments also offers some benefits specific to Hazel. For example, an environment is stored with each (evaluated) hole in the evaluation result. However, the same environment may appear many times in the evaluation result, resulting in redundant computation. This may cause an exponential time blowup when doing hole instance numbering or structural-equality checking on evaluation results [on certain Hazel programs][hole-blowup-issue]. However, with reified environments, each environment may be uniquely numbered and these tasks may be memoized by environment to avoid the redundant computations. This means that we track hole instantiations (groups of hole instances sharing the same physical environment) rather than hole instances.

While not the primary motivation, the work performed for this PHI also spawned the concept of generalized closures, which have numerous notational and practical benefits.

# Implementation

The following changes are implemented on the [`eval-environment` branch][eval-environment-branch].

## Modified evaluation rules

The `Evaluator.evaluate` function's signature is extended with a run-time environment parameter. Environments are of the type `EvalEnv.t`.

Evaluation of variable bindings adds a variable to the environment. Evaluation of variables causes a variable lookup. Functions evaluate to a function closure. Function application evaluates the body of the function using the function closure's environment extended with the argument binding. Holes evaluate to hole closures. This removes the need for substitution during evaluation.

Recursion is still implemented using the `FixF` variant, and emulates the substitution-based unwrapping semantics.

## Closure `DHExp.t` variant (generalized closures)

Previously, environments only exist in a `DHExp.t` in hole expressions, which include an environment. Now, closures may exist around functions (function closures), holes (hole closures), indeterminate unmatched expressions, or filled holes in the fill-and-resume mechanic (not implemented yet). Rather than having an optional environment argument in each of these `DHExp.t` variants, a separate `Closure` `DHExp.t` variant is added.

This is a new container expression variant that factors out the environment from any expression that may be bound to an environment. This factoring out neatens the implementation somewhat, e.g., when a hole is not evaluated and has not been bound to an expression. Note that pattern-matching against a closure should behave like pattern-matching against the enclosed expression.

We provide the following interpretation for generalized closures: they characterize the "evaluation boundary," as illustrated in [the report][report]. For any expression where evaluation has not been reached (e.g., in a function body, or in the body of an failed match expression), or evaluation may continue (i.e., holes may be "resumed" in the fill-and-resume mechanic), a closure encloses that expression.

## Postprocessing

The following tasks are implemented in the `EvalPostprocess` module.

### Substitution postprocessing

In order to make the result of evaluation with environments be the same as evaluation with substitution, we need to perform a few tasks. First, we need to remove all closures, except those around holes. All bound variables that lie within a function body (i.e., outside the evaluation boundary) should be looked up. All holes that lie within a function body (i.e., outside the evaluation boundary) should be wrapped in a hole closure.

In essence, this is performing an eager substitution pass, using the stored environments in function closures, and then discarding those environments. This is not a performance issue since we only need to do one pass over the evaluation result.

### Hole instantiation numbering

To avoid the [hole blowup issue][hole-blowup-issue], hole instances are grouped into hole instantiations. Each hole instantiation represents the group of hole instances of the same hole representing the same run-time instance ("instantiation"). An instantiation may occur multiple times in the result (multiple "instances") due to being referenced from multiple holes.

Holes are numbered by hole instantiations rather than hole instances. The numbering order is based on a simple DFS of the evaluation result as opposed to the existing BFS-like traversal, which changes the numbering order.

(The `Program.renumber` function did the hole instance numbering and hole path tracking in the existing code.)

### Hole parent tracking

Hole instances are uniquely identified by a "path" of hole parents. On the other hand, instantiations are groups of hole instances, and thus may have multiple parents. A separate postprocessing step generates a list of parent hole instantiations for each hole instantiation.

## BST representation of environments

The `EvalEnv.t` type uses the `VarBSTMap.t` type for a variable mapping, which uses a `Map`-based implementation rather than a `List`-based implementation. This should cause a speedup in variable lookups for larger environments.

## Evaluation state

Uniquely numbering environments requires a form of evaluation state. We add an extra parameter and return value to `Evaluator.evaluate` of type `EvalState.t`, which stores any such evaluation state. This state needs to include a generator for a unique environment ID.

This simple model of evaluation state can be easily extended to other stateful operations such as tracking evaluation gas.

It will also be useful to store this alongside the evaluation result when considering the fill-and-resume operation. The saved state should be the initial state for a resumed evaluation.

## Memoization of tasks on evaluation results

With numbered environments, it is easy to memoize operations on the result to prevent unnecessarily redundant operations such as in [this issue][hole-blowup-issue]. The substitution postprocessing and hole instantiation numbering steps are memoized by environment. Additionally, a `DHExp.fast_equals` method is implemented as a structural equality check that only compares environment ID's.

Note that the hole parent tracking process is not easily memoized, due to it modifying the tracked holes. Thus it is implemented as a second postprocessing step separate from the substitution postprocessing and hole instantiation tracking.

## Evaluation of failed match expressions

If pattern-matching fails on a `let` or `case` expression, then the body expression(s) are not evaluated. Similarly to function bodies, we need to wrap these failed match expressions in a closure so that substitution postprocessing will have an environment to work with.

# Progress

See [the PR][eval-environment-pr] or [the branch][eval-environment-branch]. None of the metatheory has been rigorously checked, and is left for future work.

- [X] modified evaluation rules
- [X] closure `DHExp.t` variant
- [X] (memoized) substitution postprocessing
- [X] (memoized) hole instantiation numbering
- [X] hole parent tracking
- [X] BST representation of environments
- [X] evaluation state
- [X] evaluation of failed match expressions
- [ ] clean up UI for hole instantiation parents
- [ ] checking metatheory for completeness
- [ ] formally verifying metatheory

# Resources

See [the report][report].

[hole-blowup-issue]: https://github.com/hazelgrove/hazel/issues/536
[report]: ./assets/eval-environment-report.pdf
[eval-environment-branch]: https://github.com/hazelgrove/hazel/tree/eval-environment
[eval-environment-pr]: https://github.com/hazelgrove/hazel/pull/586
