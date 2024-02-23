package dc10.scalaq

import _root_.scala.language.implicitConversions
import dc10.scala.compiler.{compile, toString}
import dc10.scala.version.`3.4.0`
import dc10.scalaq.dsl.{*, given}
import munit.FunSuite

class LinearTypesSuite extends FunSuite:

  test("val def succeed"):
    
    val ast =
      VAL("f", STRING ==@ TUPLE(BOOLEAN, STRING),
        VAL("x", STRING) ==@ (s => Tuple(true, s)))

    val obtained: String =
      ast.compile.toString["scala-3.4.0"]
      
    val expected: String =
      "val f: String => Tuple2[Boolean, String] = x => (true, x)"
      
    assertEquals(obtained, expected)

  test("val def fail"):
    
    /**
      * val f: String => Tuple2[String, String] =
      *   x => (x, x)
      */
    val ast =
      VAL("f", STRING ==@ TUPLE(STRING, STRING),
        VAL("x", STRING) ==@ (x => Tuple(x, x)))

    val expected: String =
      "Error(Linear type error)"
      

    val obtained: String =
      ast.compile.toString["scala-3.4.0"]
      
    assertEquals(obtained, expected)