# dc10-scala
A ***D**efinitional* ***C**ompiler* for generating Scala code.
 - Library for Scala 3 (JVM only)
 - Generates code for Scala 3

```scala
"com.julianpeeters" %% "dc10-scala" % "0.3.0"
```

### `dc10-scala`


Use the `dsl` to define Scala code (see available [libraries](#libraries)):

```scala
import dc10.scala.dsl.{*, given}
import scala.language.implicitConversions // for literals, e.g. "hello, world"

val snippet = 
  for
    s <- VAL("str", STRING, "hello, world")
    _ <- VAL("msg", STRING, s)
  yield ()
// snippet: IndexedStateT[ErrorF, List[Statement], List[Statement], Unit] = cats.data.IndexedStateT@5e0e2d40
```

Use the `compiler` impl to check and render code `toString` or `toVirtualFile`:

```scala
import dc10.scala.compiler.{compile, toString}
import dc10.scala.version.`3.3.1`

val result: String = snippet.compile.toString["scala-3.3.1"]
// result: String = """val str: String = "hello, world"
// val msg: String = str"""
```

### `dc10-scalaq`
Use the optional dsl to define linear and dependent types:

```scala
import dc10.scalaq.dsl.{*, given}
import scala.language.implicitConversions // for literals, e.g. 1, 2, 3

val snippet = 
  for
    l <- VAL("l", VECTOR(3, INT), Vector.of(1, 2, 3))
    _ <- VAL("m", VECTOR(6, INT), l ++ l)
  yield ()
// snippet: IndexedStateT[ErrorF, List[Statement], List[Statement], Unit] = cats.data.IndexedStateT@38673ba7
```

Use `dc10-scala` to check and render code `toString` or `toVirtualFile`:

```scala
import dc10.scala.compiler.{compile, toString}
import dc10.scala.version.`3.3.1`

val result: String = snippet.compile.toString["scala-3.3.1"]
// result: String = """val l: List[Int] = List(1, 2, 3)
// val m: List[Int] = l ++ l"""
```

### Libraries
 - [`dc10-cats-effect`](https://github.com/julianpeeters/dc10-cats-effect)
