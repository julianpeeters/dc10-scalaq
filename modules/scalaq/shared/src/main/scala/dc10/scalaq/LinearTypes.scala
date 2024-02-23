package dc10.scalaq

import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import dc10.scala.{ErrorF, Statement}
import cats.data.StateT
import dc10.scala.Symbol.Term.ValueLevel.App.App1
import dc10.scala.Symbol.Term.ValueLevel.App.AppCtor1
import dc10.scala.Symbol.Term.ValueLevel.App.AppCtor2
import dc10.scala.Symbol.Term.ValueLevel.App.AppPure
import dc10.scala.Symbol.Term.ValueLevel.App.AppVargs
import dc10.scala.Symbol.Term.ValueLevel.App.Dot1
import dc10.scala.Symbol.Term.ValueLevel.App.Dotless
import dc10.scala.Symbol.Term.ValueLevel.Blc.ForComp
import dc10.scala.Symbol.Term.ValueLevel.Lam.Lam1
import dc10.scala.Symbol.Term.ValueLevel.Lam.Lam2
import dc10.scala.Symbol.Term.ValueLevel.Var.BooleanLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.IntLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.StringLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.UnitLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.ListCtor
import dc10.scala.Symbol.Term.ValueLevel.Var.OptionCtor
import dc10.scala.Symbol.Term.ValueLevel.Var.SomeCtor
import dc10.scala.Symbol.Term.ValueLevel.Var.TupleCtor
import dc10.scala.Symbol.Term.ValueLevel.Var.UserDefinedValue

trait LinearTypes[F[_]]:

  extension [A, B, Y, Z] (domain: F[TypeExpr[A, Y]])
    @scala.annotation.targetName("fun1TLinear")
    def ==@(codomain: F[TypeExpr[B, Z]]): F[TypeExpr[A => B, Z]]

  extension [A, B, X, Y] (fa: F[ValueExpr[A, Y]])
    @scala.annotation.targetName("fun1VLinear")
    def ==@(f: ValueExpr[A, Y] => F[ValueExpr[B, X]]): F[ValueExpr[A => B, X]]

object LinearTypes:

  trait Mixins extends LinearTypes[[A] =>> StateT[ErrorF, List[Statement], A]]:

    extension [A, B, Y, Z] (domain: StateT[ErrorF, List[Statement], TypeExpr[A, Y]])
      @scala.annotation.targetName("fun1TLinear")
      def ==@(
        codomain: StateT[ErrorF, List[Statement], TypeExpr[B, Z]]
      ): StateT[ErrorF, List[Statement], TypeExpr[A => B, Z]] =
        for
          a <- domain
          b <- codomain
          t <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel[A => B, Z]](
            Term.TypeLevel.App.Infix(
              None,
              Term.TypeLevel.Lam.Function1Type(None, b.tpe.dep),
              a.tpe,
              b.tpe,
              b.tpe.dep
            )
          )
        yield TypeExpr(t)

    extension [A, B, X, Y] (fa: StateT[ErrorF, List[Statement], ValueExpr[A, Y]])
      @scala.annotation.targetName("fun1VLinear")
      def ==@(
        f: ValueExpr[A, Y] => StateT[ErrorF, List[Statement], ValueExpr[B, X]]
      ): StateT[ErrorF, List[Statement], ValueExpr[A => B, X]] =
        for
          a <- StateT.liftF[ErrorF, List[Statement], ValueExpr[A, Y]](fa.runEmptyA)
          b <- f(a.value match
            case Term.ValueLevel.App.App1(qnt, fun, arg, tpe) => ValueExpr(App1(Some(1L), fun, arg, tpe) )
            case Term.ValueLevel.App.AppCtor1(qnt, tpe, arg) => ValueExpr(AppCtor1(Some(1L), tpe, arg))
            case Term.ValueLevel.App.AppCtor2(qnt, nme, tpe, arg1, arg2) => ValueExpr(AppCtor2(Some(1L), nme, tpe, arg1, arg2))
            case Term.ValueLevel.App.AppPure(qnt, fun, arg, tpe) => ValueExpr(AppPure(Some(1L), fun, arg, tpe))
            case Term.ValueLevel.App.AppVargs(qnt, fun, tpe, vargs*) => ValueExpr(AppVargs(Some(1L), fun, tpe, vargs*))
            case Term.ValueLevel.App.Dot1(qnt, fun, arg1, arg2, tpe) => ValueExpr(Dot1(Some(1L), fun, arg1, arg2, tpe))
            case Term.ValueLevel.App.Dotless(qnt, fun, arg1, arg2, tpe) => ValueExpr(Dotless(Some(1L), fun, arg1, arg2, tpe))
            case Term.ValueLevel.Blc.ForComp(qnt, gens, ret, tpe) => ValueExpr(ForComp(Some(1L), gens, ret, tpe))
            case Term.ValueLevel.Lam.Lam1(qnt, a, b, tpe) => ValueExpr(Lam1(Some(1L), a, b, tpe))
            case Term.ValueLevel.Lam.Lam2(qnt, a1, a2, c, tpe) => ValueExpr(Lam2(Some(1L), a1, a2, c, tpe))
            case Term.ValueLevel.Var.BooleanLiteral(qnt, tpe, b) => ValueExpr(BooleanLiteral(Some(1L), tpe, b))
            case Term.ValueLevel.Var.IntLiteral(qnt, tpe, i) => ValueExpr(IntLiteral(Some(1L), tpe, i))
            case Term.ValueLevel.Var.StringLiteral(qnt, tpe, s) => ValueExpr(StringLiteral(Some(1L), tpe, s))
            case Term.ValueLevel.Var.UnitLiteral(qnt, tpe, u) => ValueExpr(UnitLiteral(Some(1L), tpe, u))
            case Term.ValueLevel.Var.ListCtor(qnt, tpe) => ValueExpr(ListCtor(Some(1L), tpe))
            case Term.ValueLevel.Var.OptionCtor(qnt, tpe) => ValueExpr(OptionCtor(Some(1L), tpe))
            case Term.ValueLevel.Var.SomeCtor(qnt, tpe) => ValueExpr(SomeCtor(Some(1L), tpe))
            case Term.ValueLevel.Var.TupleCtor(qnt, tpe) => ValueExpr(TupleCtor(Some(1L), tpe))
            case Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl) => ValueExpr(UserDefinedValue(Some(1L), nme, tpe, impl))
          )
          t <- StateT.pure[ErrorF, List[Statement], Term.TypeLevel[A => B, X]](
            Term.TypeLevel.App.Infix(None, Term.TypeLevel.Lam.Function1Type(None, a.value.tpe.dep), a.value.tpe, b.value.tpe, b.value.tpe.dep)
          )
          v <- StateT.pure[ErrorF, List[Statement], Term.ValueLevel[A => B, X]](Term.ValueLevel.Lam.Lam1(None, a.value, b.value, t))
        yield ValueExpr(v)
