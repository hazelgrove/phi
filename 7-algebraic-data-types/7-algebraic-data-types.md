**By: Eric Griffis**

## History

| Date        |       Version |
|-------------|---------------|
| Jan 14 2021 | Initial Draft |

## Introduction

This is a proposal to introduce algebraic data types.

## Motivation

Algebraic data types are ubiquitous among functional programming languages with pattern matching, especially for ML-like languages and their descendants. 

- they are familiar to function programmers
- they are necessary for 

### Illustrative Examples

- enumerative: Bool, Planet
- composite: Int option, 
- recursive: Peano numbers, Int list
- pattern matching
- exhaustiveness and redundancy checks

### Motivating Examples

- Peano numbers (Shmyth: let y = y - 1 in ...)

## Design

- correctness
- "feel and fit" with existing core features
- performance considerations?
- naming considerations
- API footprint
- potential conflicts with existing features

## Implementation

- must be bundled with core (no module system)
- any existing implementations?
- time frame
- roll-out plan
- other contributors?

## Counter Examples

- What are they not intended to be used for?
- Examples of what to avoid and why

## Drawbacks

Why should we *not* do this?

## Alternatives

- What other possibilities have been examined?
- What is the impact of not implementing this proposal?

## References

1. [existing projects][1]
2. [any API documentation][2]
3. [academic research papers or supporting material][3]
4. [alternative libraries / implementations][4]
5. [any related discussion][5]

[1]: https://github.com "GitHub"
[2]: https://hazel.org "Hazel"
[3]: https://hazel.org "Hazel"
[4]: https://github.com "GitHub"
[5]: https://slack.com "Slack"