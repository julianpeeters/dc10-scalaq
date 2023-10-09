# dc10-scala
A ***D**efinitional* ***C**ompiler* for generating Scala code.
 - [`dc10-scala`](#dc10-scala): AST and dsl for defining and rendering Scala programs

### Getting Started
 - Libraries for Scala 3 (JVM only)
 - Generates code for Scala 3

```scala
"com.julianpeeters" %% "dc10-<module>" % "0.2.0"
```

### Usage

#### `dc10-scala`

Use the dsl to define Scala code:

```scala
import cats.data.StateT
import dc10.scala.ast.Statement
import dc10.scala.dsl.{*, given}
import dc10.scala.error.ErrorF
import scala.language.implicitConversions // for literals, e.g. "hello, world"

val snippet: StateT[ErrorF, List[Statement], Unit] = 
  for
    s <- VAL("str", STRING, "hello, world")
    _ <- VAL("msg", STRING, s)
  yield ()
```

Use the compiler impl to check and render code to `String` or `VirtualFile`:

```scala
import dc10.scala.compiler.{compile, toString}
import dc10.scala.version.`3.3.1`

val result: String = snippet.compile.toString["scala-3.3.1"]
// result: String = """val str: String = "hello, world"
// val msg: String = str"""
```