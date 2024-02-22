package dc10.scalaq

import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.{ErrorF, Statement}
import cats.data.StateT



trait Pi[F[_]]:

  extension [A, B, X, Y, Z] (fa: F[ValueExpr[A, Y]])
    @scala.annotation.targetName("fun1VT")
    def ==>(f: ValueExpr[A, Y] => F[TypeExpr[B, Z]]): F[TypeExpr[A => B, Z]]

object Pi:

  trait Mixins extends Pi[[A] =>> StateT[ErrorF, List[Statement], A]]:
 
    extension [A, B, X, Y, Z] (fa: StateT[ErrorF, List[Statement], ValueExpr[A, Y]])
      @scala.annotation.targetName("fun1VT")
      def ==>(f: ValueExpr[A, Y] => StateT[ErrorF, List[Statement], TypeExpr[B, Z]]): StateT[ErrorF, List[Statement], TypeExpr[A => B, Z]] =
        for
          a <- StateT.liftF(fa.runEmptyA)
          b <- f(a)
          t <- StateT.pure(
            Term.TypeLevel.App.InfixPi(
              None,
              Term.TypeLevel.Lam.Function1Type(None, a.value.tpe.dep),
              a.value,
              b.tpe,
              b.tpe.dep
            )
          )

        yield TypeExpr(t)
