# dc10-scala
A ***D**efinitional* ***C**ompiler* for generating Scala code.
 - Library for Scala 3 (JVM only)
 - Generates code for Scala 3

```scala
"com.julianpeeters" %% "dc10-scala" % "0.3.0"
```

### `dc10-scala`


1. Use the `dsl` to define Scala code (see available [libraries](#libraries)):

    ```scala mdoc
    import dc10.scala.dsl.{*, given}
    import scala.language.implicitConversions // for literals, e.g. "hello, world"

    val snippet = 
      for
        s <- VAL("str", STRING, "hello, world")
        _ <- VAL("msg", STRING, s)
      yield ()
    ```

2. Use the `compiler` impl to check and render code `toString` or `toVirtualFile`:

    ```scala mdoc
    import dc10.scala.compiler.{compile, toString}
    import dc10.scala.version.`3.3.1`

    val result: String = snippet.compile.toString["scala-3.3.1"]
    ```

### `libraries`
 - [`dc10-cats-effect`](https://github.com/julianpeeters/dc10-cats-effect)
