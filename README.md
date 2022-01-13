# TraceChecker

TraceChecker is a toolset for performing lightweight formal verification on execution traces.
Intended for use in an educational context, this tool aims to make it feasible to:

1. Give students assignments where the solution might be highly non-deterministic,
   while retaining the ability to auto-grade work consistently and precisely.
2. Provide precise explanations of what properties students did or did not satisfy,
   using students' own log files as point of reference.

For more precise explanations of individual operators, see the scaladoc comments.
For a higher-level overview, read on.

## Design and Usage Overview

The current version of the project provides a DSL for expressing "Queries".
The DSL allows the construction of objects of type `Query[T]`, which encapsulate the logic necessary to either compute a value of type `T`, or indicate what went wrong when trying to do so.

For functional programmers, `Query[_]` is a fairly standard state + error monad with built-in features for expressing maximally readable error messages.

A `Query[T]`'s execution maintains facilities for annotating values of interest, e.g:
```scala
interestingQuery.label("name").flatMap { interestingValue =>
  reject("something went wrong")
}
```

Running this query, assuming `interestingQuery` succeeds, will produce a result that looks like this:
```
name := pretty-printed interestingValue
  something went wrong at filename:linenumber
```

If a student were given the spec file, this error, and one of their traces, the positional information and contextual information should ensure errors are interpretable.
Just providing a list of name-value bindings will not scale-however.
For more complex queries, it is also possible to indicate nesting.
Consider if the previous example was defined as a helper:

```scala
val helper: Query[T] = interestingQuery.label("name").flatMap { interestingValue =>
   reject("something went wrong")
}
```

Since `helper` might be referenced from multiple places, we should indicate where we came from if we fail, which can be done with the `call` combinator:
```scala
call(helper)
```

This will result in an annotation listing the `filename:line number` of that callsite as context:
```
filename:callsiteline:
  name := pretty-printed interestingValue
    something went wrong at filename: problemline
```

This structure can be nested arbitrarily, allowing queries of any complexity to provide stacktrace-like error descriptions of what went wrong.

Based on this framework, rulesets applying to distributed traces can be specified using `Spec[E <: Element]` objects.

```scala
// once we define a sealed abstract root element type,
// defining the tracing format is as simple as
// listing out case class definitions
sealed abstract class Record extends Element

final case class ServerStart() extends Record
final case class ServerEcho(kill: Boolean) extends Record
final case class ServerStop() extends Record

final case class ClientStart(kill: Boolean, requestCount: Int) extends Record
final case class ClientSend() extends Record
final case class ClientReceive() extends Record
final case class ClientStop() extends Record

// Spec[Record] invokes macros to auto-generate a JSON-to-Record parser
object Specification extends Spec[Record] {
   import Specification._

   // you can define any helpers you need using normal Scala idioms
   // this extractor is used to look for distributed traces
   // containing ClientStart entries (we defined ClientStart above)
   object ContainsClientStart {
      def unapply(trace: List[Record]): Option[ClientStart] =
         trace.collectFirst { case cs@ClientStart(_, _) => cs }
   }
   
   // the specification itself can be given as a collection of rules
   // each rule's body is a Query[Any], which will be evaluated
   // in order to determine if the rule holds
   val rootRule: RootRule = RootRule(
      // define a rule called r1
      rule("r1") {
         // Specification provides access to a pre-loaded set of traces,
         // which are pairs of (trace identified, List[Record])
         traces.quantifying("trace").forall {
            // this quantification only applies to traces containing
            // ClientStart; you can limit scope using any pattern you like
            case (id, trace@ContainsClientStart(clientStart)) =>
               // Query works with for-expressions, which are just
               // combinations of flatMap and map method invocations
               for {
                  // record important info
                  _ <- label("id")(id)
                  _ <- label("clientStart")(clientStart)
                  _ <- accept(trace.collect { case cs@ClientStart(_, _) => cs })
                          // several helpers are provided for common use cases, like
                          // expecting one of a certain thing
                          .requireOne
                  // arbitrarily complex logic can be nested to any depth
                  _ <- exists("clientStop")(trace) {
                     case clientStop@ClientStop() =>
                        // assertions require human-readable descriptions,
                        // to make sure errors contain some human-readable wording
                        require("clientStop happens-after clientStart")(clientStart <-< clientStop)
                  }
               } yield ()
         }
      },
   )
}

// Specification.checkRules(os.Path...) will check the ruleset defined
// in Specification against the trace(s) provided, yielding an
// inspectable result object
```

### Intended as plain-text format for distribution

The interpretability of spec outputs relies on the specification code being accessible.
For this, you can use [Ammonite](https://ammonite.io/Ammonite) for its scripting capabilities and provide complete readable specification scripts to students.

To import the latest version in Ammonite in such a script, use the following [Magic Imports](https://ammonite.io/#MagicImports):
```scala
import $repo.`https://jitpack.io`
import $ivy.`com.github.DistributedClocks:tracechecker:master-SNAPSHOT`
```

This relies on [JitPack](https://jitpack.io), a free service allowing Java and Scala projects to be directly imported from Github via Ivy.
Note that the given imports track this repository's master branch, which may or may not be what you want.
We're using it like this to avoid students having to change version numbers if we publish a bugfix mid-assignment.
