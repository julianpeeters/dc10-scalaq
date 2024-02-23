# dc10-scalaq

Experimental extensions and alternative DSL to the `dc10-scala` code generator.
 - Library for Scala 3 (JS, JVM, and Native platforms)
 - Generates code for Scala 3

```scala
"com.julianpeeters" %% "dc10-scalaq" % "@VERSION@"
```

## Dependent Vector Types

```scala mdoc:reset
import dc10.scalaq.dsl.{*, given}
import scala.language.implicitConversions // for literals, e.g. 1, 2, 3

val snippet = 
  for
    l <- VAL("l", VECTOR(3, INT), Vector.of(1, 2, 3))
    _ <- VAL("m", VECTOR(6, INT), l ++ l)
  yield ()
```

Use the `compiler` to typecheck, then render code as `List`:

```scala mdoc
import dc10.scala.compiler.{compile, toString}
import dc10.scala.version.`3.4.0`

val result: String = snippet.compile.toString["scala-3.4.0"]
```

## Linear Types

```scala mdoc:reset
import dc10.scalaq.dsl.{*, given}
import scala.language.implicitConversions // for reference to x

val snippet =
  VAL("f", STRING ==@ TUPLE(STRING, STRING),
    VAL("x", STRING) ==@ (x => Tuple(x, x)))
```

Use the `compiler` to typecheck, then render code as `Function`:

```scala mdoc
import dc10.scala.compiler.{compile, toString}
import dc10.scala.version.`3.4.0`

val result: String = snippet.compile.toString["scala-3.4.0"]
```