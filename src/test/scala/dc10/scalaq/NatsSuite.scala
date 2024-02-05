package dc10.scalaq


import _root_.scala.language.implicitConversions
import cats.implicits.given
import dc10.scala.compiler.{compile, toString}
import dc10.scala.version.`3.3.1`
import dc10.scalaq.dsl.{*}
import munit.FunSuite

class NatsSuite extends FunSuite:
  
  test("Types parameterized by Nat values"):

    def ast =
      for
        _ <- VAL("a", EVEN(Zero))
        _ <- VAL("b", EVEN(Succ(Succ(Zero))))
        _ <- VAL("c", ODD(Succ(Zero)))
      yield ()
    
    val obtained: String =
      ast.compile.toString["scala-3.3.1"]
          
    val expected: String =
      """|val a: Even
         |val b: Even
         |val c: Odd""".stripMargin
      
    assertEquals(obtained, expected)
