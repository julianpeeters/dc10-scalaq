package dc10.scalaq

import cats.data.StateT
import cats.implicits.given
import dc10.scala.dsl.{==>, refV}
import dc10.scala.{Error, ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import org.tpolecat.sourcepos.SourcePos
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

type VectorN[_]

trait Vectors[F[_]]:
  def VECTOR[A]: F[TypeExpr[VectorN[A], Unit]]
  def Vector[A]: F[ValueExpr[VectorN[A], Unit]]
  extension [A] (tfunction: F[TypeExpr[VectorN[A], Unit]])
    @scala.annotation.targetName("app1TQ")
    def apply[Z](len: F[ValueExpr[Int, Unit]], targs: F[TypeExpr[A, Z]]): F[TypeExpr[VectorN[A], (Int, Z)]]
  extension [A] (ctor: F[ValueExpr[VectorN[A], Unit]])
    @scala.annotation.targetName("appVQ1")
    def of[Z](args: F[ValueExpr[A, Z]]*)(using sp: SourcePos): F[ValueExpr[VectorN[A], (Int, Z)]]
  extension [Z, A] (v1: F[ValueExpr[VectorN[A], (Int, Z)]])
    def ++(v2: F[ValueExpr[VectorN[A], (Int, Z)]])(using sp: SourcePos): F[ValueExpr[VectorN[A], (Int, Z)]]

object Vectors:
 
  trait Mixins extends Vectors[[A] =>> StateT[ErrorF, List[Statement], A]]:

    def VECTOR[A]: StateT[ErrorF, List[Statement], TypeExpr[VectorN[A], Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.UserDefinedType(None, "List", None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()))))
      
    def Vector[A]: StateT[ErrorF, List[Statement], ValueExpr[VectorN[A], Unit]] =
      for
        t <- VECTOR[A]
        v <- StateT.pure(ValueExpr(Term.ValueLevel.Var.UserDefinedValue(None, "List", t.tpe, None)))
      yield v
    
    extension [A] (tfunction: StateT[ErrorF, List[Statement], TypeExpr[VectorN[A], Unit]])
      @scala.annotation.targetName("app1TQ")
      def apply[Z](len: StateT[ErrorF, List[Statement], ValueExpr[Int, Unit]], targs: StateT[ErrorF, List[Statement], TypeExpr[A, Z]]): StateT[ErrorF, List[Statement], TypeExpr[VectorN[A], (Int, Z)]] =
        for
          f <- tfunction
          a <- targs
          l <- len
        // yield TypeExpr(Term.TypeLevel.App.App1(None, f.tpe.manageDep(_ => l.value), a.tpe, (l.value, a.tpe.dep)))
        // yield TypeExpr(Term.TypeLevel.App.App1(None, f.tpe, a.tpe, (l.value, a.tpe.dep)))
        yield
          TypeExpr(Term.TypeLevel.App.App1(None, f.tpe, a.tpe,
            dep = Term.ValueLevel.App.AppCtor2(None, "",// Term.ValueLevel.Var.TupleCtor(
          //  None,
            Term.TypeLevel.App.App2(
              None,
              Term.TypeLevel.Var.TupleType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())),
              l.value.tpe,
              a.tpe.dep.tpe,
              // Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())
              f.tpe.dep.tpe.dep
            ),
            // Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
            l.value,
            Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())
          )
          ))
        
    extension [A] (ctor: StateT[ErrorF, List[Statement], ValueExpr[VectorN[A], Unit]])
      @scala.annotation.targetName("appVQ1")
      def of[Z](args: StateT[ErrorF, List[Statement], ValueExpr[A, Z]]*)(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[VectorN[A], (Int, Z)]] =
        for
          l <- ctor
          a <- args.toList.sequence
          h <- StateT.liftF[ErrorF, List[Statement], ValueExpr[A, Z]](a.headOption.toRight(List(Error(s"value level application error"))))
          v <- StateT.liftF[ErrorF, List[Statement], Term.ValueLevel[VectorN[A], (Int, Z)]](
            if (a.forall(e => e.value.tpe.dep == h.value.tpe.dep))
            then Right(
              Term.ValueLevel.App.AppVargs[VectorN, A, Int, Z](
                None,
                // l.value.manageDep(_ => Term.ValueLevel.Var.IntLiteral(None, Term.TypeLevel.Var.IntType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())), a.length)),
                l.value.manageDep(_ => Term.ValueLevel.Var.IntLiteral(None, Term.TypeLevel.Var.IntType(None, h.value.tpe.dep.tpe.dep), a.length)),
                l.value.tpe.manageDep(_ =>
                  Term.ValueLevel.App.AppCtor2(None, "",
                  //  None,
                    // ???,
                      Term.TypeLevel.App.App2(
                        None,
                        Term.TypeLevel.Var.TupleType(None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())),
                        Term.TypeLevel.Var.IntType(None, h.value.tpe.dep.tpe.dep),
                        h.value.tpe.dep.tpe,
                        // Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ())
                        h.value.tpe.dep.tpe.dep
                    ),
                    Term.ValueLevel.Var.IntLiteral(None, Term.TypeLevel.Var.IntType(None, h.value.tpe.dep.tpe.dep), a.length),
                    Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()),
                  )
                ),
                a.map(arg => arg.value)*
              )
            )
            else Left(List(Error(s"${sp.file}:${sp.line}\nDependent Vector error")))
          )
        yield ValueExpr(v)

    extension [Z, A] (vector1: StateT[ErrorF, List[Statement], ValueExpr[VectorN[A], (Int, Z)]])
      def ++(vector2: StateT[ErrorF, List[Statement], ValueExpr[VectorN[A], (Int, Z)]])(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[VectorN[A], (Int, Z)]] =
        for
          o <- vector1
          v <- vector2
          f <- o.value match
            case Term.ValueLevel.Var.UserDefinedValue(qnt, nme, tpe, impl) =>
              for
                i <- StateT.liftF[ErrorF, List[Statement], Seq[Term.ValueLevel[A, Z]]](o.value.findVargs.toRight(List(Error("no vargs in this"))))
                w <- StateT.liftF[ErrorF, List[Statement], Seq[Term.ValueLevel[A, Z]]](v.value.findVargs.toRight(List(Error("no vargs in that"))))
                n = i.appendedAll(w)
                g <- vector2 ==> ((s: ValueExpr[VectorN[A], (Int, Z)]) => Vector.of(n.map(e => refV(ValueExpr(e)))*))
                v <- StateT.pure[ErrorF, List[Statement], ValueExpr[VectorN[A] => VectorN[A], (Int, Z)]](
                  ValueExpr(Term.ValueLevel.Var.UserDefinedValue(None, "++", g.value.tpe.manageDep(_ => tpe.dep), Some(g.value.manageDep(_ => tpe.dep))))
                )
              yield v
            case _ => StateT.liftF[ErrorF, List[Statement], ValueExpr[VectorN[A] => VectorN[A], (Int, Z)]](Left(List(Error(s"${sp.file}:${sp.line}\nAppend error"))))
        yield ValueExpr(Term.ValueLevel.App.Dotless(None, f.value, o.value, v.value, o.value.tpe.manageDep(d =>

          // (
          //   d._1 + v.value.tpe.dep._1,
          //   d._2
          // )
          (d, v.value.tpe.dep) match
            case (App1(qnt, fun, arg, tpe), _) => ???
            case (AppCtor1(qnt, tpe, arg), _) => ???
            case (AppCtor2(qntA, nmeA, tpeA, arg1A, arg2A), AppCtor2(qntB, nmeB, tpeB, arg1B, arg2B)) => AppCtor2(qntA, nmeA, tpeA,
              // arg1,
              // arg2

              (arg1A, arg1B) match
                case (App1(qnt, fun, arg, tpe), _) => ???
                case (AppCtor1(qnt, tpe, arg), _) => ???
                case (AppCtor2(qnt, nme, tpe, arg1, arg2), _) => ???
                case (AppPure(qnt, fun, arg, tpe), _) => ???
                case (AppVargs(qnt, fun, tpe, vargs*), _) => ???
                case (Dot1(qnt, fun, arg1, arg2, tpe), _) => ???
                case (Dotless(qnt, fun, arg1, arg2, tpe), _) => ???
                case (ForComp(qnt, gens, ret, tpe), _) => ???
                case (Lam1(qnt, a, b, tpe), _) => ???
                case (Lam2(qnt, a1, a2, c, tpe), _) => ???
                case (BooleanLiteral(qnt, tpe, b), _) => ???
                case (IntLiteral(qnt1, tpe1, i1), IntLiteral(qnt2, tpe2, i2)) => IntLiteral(qnt1, tpe1, i1 + i2)
                case (IntLiteral(qnt1, tpe1, i1), _) => ???
                case (StringLiteral(qnt, tpe, s), _) => ???
                case (UnitLiteral(qnt, tpe, u), _) => ???
                case (ListCtor(qnt, tpe), _) => ???
                case (OptionCtor(qnt, tpe), _) => ???
                case (SomeCtor(qnt, tpe), _) => ???
                case (TupleCtor(qnt, tpe), _) => ???
                case (UserDefinedValue(qnt, nme, tpe, impl), _) => ???
              ,
              arg2A
            )
            case (AppCtor2(qntA, nmeA, tpeA, arg1A, arg2A), _) => ???
            case (AppPure(qnt, fun, arg, tpe), _) => ???
            // case AppVargs(qnt, fun, tpe, vargs*) => ???
            case (Dot1(qnt, fun, arg1, arg2, tpe), _) => ???
            case (Dotless(qnt, fun, arg1, arg2, tpe), _) => ???
            case (ForComp(qnt, gens, ret, tpe), _) => ???
            // case Lam1(qnt, a, b, tpe) => ???
            // case Lam2(qnt, a1, a2, c, tpe) => ???
            // case BooleanLiteral(qnt, tpe, b) => ???
            // case IntLiteral(qnt, tpe, i) => ???
            // case StringLiteral(qnt, tpe, s) => ???
            // case UnitLiteral(qnt, tpe, u) => ???
            case (ListCtor(qnt, tpe), _) => ???
            case (OptionCtor(qnt, tpe), _) => ???
            case (SomeCtor(qnt, tpe), _) => ???
            case (TupleCtor(qnt, tpe), _) => ???
            case (UserDefinedValue(qnt, nme, tpe, impl), _) => ???
          

        )))