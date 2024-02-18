package dc10.scalaq

import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.{ErrorF, Statement}
import cats.data.StateT


trait Pi[F[_]]:

  extension [A, B, X, Y, Z] (fa: F[ValueExpr[A, Y]])
    @scala.annotation.targetName("fun1VT")
    def ==>(f: ValueExpr[A, Y] => F[TypeExpr[B, Z]]): F[TypeExpr[B, Z]]


object Pi:

  trait Mixins extends Pi[[A] =>> StateT[ErrorF, List[Statement], A]]:
 
    extension [A, B, X, Y, Z] (fa: StateT[ErrorF, List[Statement], ValueExpr[A, Y]])
      @scala.annotation.targetName("fun1VT")
      def ==>(f: ValueExpr[A, Y] => StateT[ErrorF, List[Statement], TypeExpr[B, Z]]): StateT[ErrorF, List[Statement], TypeExpr[B, Z]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- f(a)
          // t <- StateT.pure[ErrorF, List[Statement], TypeLevel[B, Z]](Term.TypeLevel.App.App3(None, Term.TypeLevel.Lam.Function2Type(None, a._1.value.tpe.dep), a._1.value.tpe, a._2.value.tpe, b.value.tpe, a._2.value.tpe.dep))
        yield b
