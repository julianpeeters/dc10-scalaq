package dc10.scalaq

import _root_.scala.language.implicitConversions
import cats.implicits.given
import dc10.scala.compiler.{compile, toString}
import dc10.scala.version.`3.4.0`
import dc10.scalaq.dsl.*
import munit.FunSuite
// import dc10.scalaq.numbers.Nat

class NatsSuite extends FunSuite:
  
  test("Types parameterized by Nat values"):

    def ast =
      for
        _ <- VAL("a", EVEN(Zero))
        _ <- VAL("b", EVEN(Succ(Succ(Zero))))
        _ <- VAL("c", ODD(Succ(Zero)))
      yield ()
    
    val obtained: String =
      ast.compile.toString["scala-3.4.0"]
          
    val expected: String =
      """|val a: Even
         |val b: Even
         |val c: Odd""".stripMargin
      
    assertEquals(obtained, expected)
  
  test("Types parameterized by Even values"):

    def ast =
      for
        _ <- VAL("a1", EVEN(Zero))
        _ <- VAL("b1", EVEN(Succ(Succ(Zero))))
        _ <- VAL("a2", EVEN2(Zero))
        _ <- VAL("b2", EVEN2(Succ(Succ(Zero))))
      yield ()
    
    val obtained: String =
      ast.compile.toString["scala-3.4.0"]
          
    val expected: String =
      """|val a1: Even
         |val b1: Even
         |val a2: Even2
         |val b2: Even2""".stripMargin
      
    assertEquals(obtained, expected)