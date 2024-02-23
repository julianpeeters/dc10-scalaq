# dc10-scalaq

Experimental extensions and alternative DSL to the `dc10-scala` code generator.
 - Library for Scala 3 (JS, JVM, and Native platforms)
 - Generates code for Scala 3

```scala
"com.julianpeeters" %% "dc10-scalaq" % "0.6.0"
```

## Dependent Vector Types

```scala
import dc10.scalaq.dsl.{*, given}
import scala.language.implicitConversions // for literals, e.g. 1, 2, 3

val snippet = 
  for
    l <- VAL("l", VECTOR(3, INT), Vector.of(1, 2, 3))
    _ <- VAL("m", VECTOR(6, INT), l ++ l)
  yield ()
// snippet: IndexedStateT[ErrorF, List[Statement], List[Statement], Unit] = cats.data.IndexedStateT@73036f97
```

Use the `compiler` to typecheck, then render code as `List`:

```scala
import dc10.scala.compiler.{compile, toString}
import dc10.scala.version.`3.4.0`

val result: String = snippet.compile.toString["scala-3.4.0"]
// result: String = """val l: List[Int] = List(1, 2, 3)
// val m: List[Int] = l ++ l"""
```

## Linear Types

```scala
import dc10.scalaq.dsl.{*, given}
import scala.language.implicitConversions // for reference to x

val snippet =
  VAL("f", STRING ==@ TUPLE(STRING, STRING),
    VAL("x", STRING) ==@ (x => Tuple(x, x)))
// snippet: IndexedStateT[ErrorF, List[Statement], List[Statement], ValueExpr[Function1[String, Tuple2[String, String]], Tuple2[Unit, Tuple2[Unit, Unit]]]] = cats.data.IndexedStateT@6167066a
```

Use the `compiler` to typecheck, then render code as `Function`:

```scala
import dc10.scala.compiler.{compile, toString}
import dc10.scala.version.`3.4.0`

val result: String = snippet.compile.toString["scala-3.4.0"]
// result: String = "Error(Linear type error)"
```