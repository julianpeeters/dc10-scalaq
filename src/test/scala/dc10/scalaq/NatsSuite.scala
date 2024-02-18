package dc10.scalaq

// import _root_.scala.language.implicitConversions
// import cats.implicits.given
// import dc10.scala.compiler.{compile, toString}
// import dc10.scala.version.`3.3.1`
// import dc10.scalaq.`1.7.1+`
// import dc10.scalaq.dsl.{*, given}
// import munit.FunSuite
// import dc10.scalaq.numbers.Nat

object NatsSuite
// class NatsSuite extends FunSuite:
  
//   test("Types parameterized by Nat values"):

//     def ast =
//       for
//         _ <- VAL("a", EVEN(Zero))
//         _ <- VAL("b", EVEN(Succ(Succ(Zero))))
//         _ <- VAL("c", ODD(Succ(Zero)))
//       yield ()
    
//     val obtained: String =
//       ast.compile.toString["scala-3.3.1"]
          
//     val expected: String =
//       """|val a: Even
//          |val b: Even
//          |val c: Odd""".stripMargin
      
//     assertEquals(obtained, expected)
  
//   test("Types parameterized by Even values"):

//     def ast =
//       for
//         _ <- VAL("a1", EVEN(Zero))
//         _ <- VAL("b1", EVEN(Succ(Succ(Zero))))
//         _ <- VAL("a2", EVEN2(Zero))
//         _ <- VAL("b2", EVEN2(Succ(Succ(Zero))))
//       yield ()
    
//     val obtained: String =
//       ast.compile.toString["scala-3.3.1"]
          
//     val expected: String =
//       """|val a1: Even
//          |val b1: Even
//          |val a2: Even2
//          |val b2: Even2""".stripMargin
      
//     assertEquals(obtained, expected)

//   test("Scala declaration of even-implies-even2"):

//     def ast =
//       for
//         n <- VAL("n", NAT)
//         _ <- TYPE("evenImpliesEven2", EVEN(n) ==> EVEN2(n))
//       yield ()
    
//     val obtained =
//       ast.compile.toString["scala-3.3.1"]
          
//     val expected: String =
//       """|val n: Nat
//          |val evenImpliesEven2: Even => Even2""".stripMargin
      
//     assertEquals(obtained, expected)


//   test("Twelf declaration of even-implies-even2"):

//     def ast =
//       TYPE("evenImpliesEven2", VAL("n", NAT) ==> (n => EVEN(n) ==> EVEN2(n)))

//     val obtained =
//       ast.compile.toString["Twelf 1.7.1+"]
          
//     val expected =
//       "even-implies-even2 : {N:nat} -> even N -> even2 N"
      
//     assertEquals(obtained, expected)