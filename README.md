# TraceChecker

TraceChecker is a toolset for performing lightweight formal verification on execution traces.
Intended for use in an educational context, this tool aims to make it feasible to:

1. Give students assignments where the solution might be highly non-deterministic,
   while retaining the ability to auto-grade work consistently and precisely.
2. Provide precise explanations of what properties students did or did not satisfy,
   using students' own log files as point of reference.

For more precise explanations of individual operators, see the scaladoc comments.
For a higher-level overview, read on.

## Rationale

The current version of this project takes heavy inspiration from Scala's parser combinators, considering
verification to be a variation on the parsing problem.
A log of actions is given as inputs, and it is linearly scanned in order to determine whether it contains
correct or incorrect patterns.
Patterns may depend on each other, and a successfully matching pattern doubles as an arbitrary query against
the log.

A notable difference from parser combinators is that, unlike parsing, where there at every stage there is
generally one "answer" (the desired AST, or an error), verification conditions may be highly ambiguous,
and all combinations of a set of patterns must generally be checked.
As a result, each `TraceChecker[T]` will yield an arbitrary number of results, rather than just one.

Additionally, in order to be able to provide complete explanations (counter-examples, essentially) for any
failures that are detected, a running checker will generate a tree of human-readable text representing
its path through the log, alongside any relevant control flow.
This is, alongside a basic pass/fail, is the primary output of a checker: an explanation of what does and
does not work.

## Usage example

```scala
guards(
  withLabel(d"$PowlibMiningBegin must be first relevant action logged") {
    eventually(check(d"first relevant action")())
      .first
      .require(d"should be $PowlibMiningBegin")(_.tag == PowlibMiningBegin)
  },
  withLabel(d"exactly one $PowlibMiningBegin, $PowlibMiningComplete pair, and $PowlibMiningComplete is not followed by any actions") {
    once(eventually(check(PowlibMiningBegin.toDescription)(_.tracerId == runInfo.clientName, _.tag == PowlibMiningBegin))) ~>
      once(eventually(check(PowlibMiningComplete.toDescription)(_.tracerId == runInfo.clientName, _.tag == PowlibMiningComplete))) ~>
      not(eventually(check(d"any actions after PowlibMiningComplete")()))
  })
```

The example above illustrates usage of the checker to express some properties.
At the top level, two properties are being jointly verified, using the `guards` and `withLabel`
to (1) branch verification into two different paths and (2) provide human-readable labels indicating the
purpose of each path.

The first property expresses that `eventually` there should be a context that passes the `check`, and that,
if so, the `first` success is `require`d to yield a log element whose `tag` field is equal to the
constant `PowlibMiningBegin`.

The second property expresses that `eventually` there should be a log element that passes the `check` on the
first line (which constraints multiple fields of a given log element via equalities), and that this
eventuality should occur exactly `once`.
Then (symbolically represented as `~>`), a further log element (reading from the location at which the previous
property was satisfied, to the end of the log) should `eventually` satisfy the `check` on the second line,
and that this eventuality should also be satisfied exactly `once`.
If both of these properties are satisfied, then no further relevant actions may occur in the log past
`PowlibMiningComplete`.
This condition is expressed using an equivalent negation, that a `check` for further log elements should
`not` `eventually` be satisfied.

### Checking the property

Any condition, such as the one above, will have the type `TraceChecker[T]`, for some `T`, depending on the
condition's structure.
`TraceChecker`s are callable, and checking can be done like so, using a `Scanner` (a sequence type optimised
for use by a `TraceChecker`) as input:
```scala
traceChecker(scanner) match {
  case TracePassed(desc, _) =>
    println(s"success! ${desc.linesIterator.mkString("\n")}")
  case TraceViolation(desc) =>
    println(s"failure! ${desc.linesIterator.mkString("\n")}")
}
```
