# dc10-scalaq

Experimental extensions to the `dc10-scala` code generator.
 - Library for Scala 3 (JVM only)
 - Generates code for Scala 3

```scala
"com.julianpeeters" %% "dc10-scalaq" % "0.5.0"
```

### `dc10-scalaq`
An alternative `dc10-scala` DSL with dependent vector types:

```scala
import dc10.scalaq.dsl.{*, given}
import scala.language.implicitConversions // for literals, e.g. 1, 2, 3

val snippet = 
  for
    l <- VAL("l", VECTOR(3, INT), Vector.of(1, 2, 3))
    _ <- VAL("m", VECTOR(6, INT), l ++ l)
  yield ()
// snippet: IndexedStateT[ErrorF, List[Statement], List[Statement], Unit] = cats.data.IndexedStateT@1fa6cf9
```

Use the `compiler` to typecheck, then render code as `List`:

```scala
import dc10.scala.compiler.{compile, toString}
import dc10.scala.version.`3.3.1`

val result: String = snippet.compile.toString["scala-3.3.1"]
// result: String = """val l: List[Int] = List(1, 2, 3)
// val m: List[Int] = l ++ l"""
```