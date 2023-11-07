# dc10-scalaq

Experimental extensions to the `dc10-scala` code generator.
 - Library for Scala @SCALA@ (JVM only)
 - Generates code for Scala @SCALA@

```scala
"com.julianpeeters" %% "dc10-scalaq" % "@VERSION@"
```

### `dc10-scalaq`
An alternative `dc10-scala` DSL with dependent vector types:

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
import dc10.scala.version.`3.3.1`

val result: String = snippet.compile.toString["scala-3.3.1"]
```