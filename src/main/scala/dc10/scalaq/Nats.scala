package dc10.scalaq

import cats.data.StateT
import cats.implicits.given
// import dc10.scala.dsl.{==>, refV}
import dc10.scala.{Error, ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import org.tpolecat.sourcepos.SourcePos
import dc10.scala.Symbol.Term.TypeLevel.Var.UserDefinedType
import dc10.scala.Symbol.Term.ValueLevel.App.AppCtor1
import dc10.scala.Symbol.Term.ValueLevel.App.AppCtor2
import dc10.scala.Symbol.Term.ValueLevel.App.AppPure
import dc10.scala.Symbol.Term.ValueLevel.App.Dot1
import dc10.scala.Symbol.Term.ValueLevel.App.Dotless
import dc10.scala.Symbol.Term.ValueLevel.Lam.Lam1
import dc10.scala.Symbol.Term.ValueLevel.Lam.Lam2
import dc10.scala.Symbol.Term.ValueLevel.Var.BooleanLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.IntLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.StringLiteral
import dc10.scala.Symbol.Term.ValueLevel.Var.ListCtor
import dc10.scala.Symbol.Term.ValueLevel.Var.OptionCtor
import dc10.scala.Symbol.Term.ValueLevel.Var.SomeCtor
import dc10.scala.Symbol.Term.ValueLevel.Var.UserDefinedValue
import dc10.scala.Symbol.Term.ValueLevel

type Nat
type Even
type Odd

trait Nats[F[_]]:
  def NAT: F[TypeExpr[Nat, Unit]]
  def Zero: F[ValueExpr[Nat, Unit]]
  def Succ: F[ValueExpr[Nat, Unit]]
  def EVEN: F[TypeExpr[Even, Unit]]
  def Even: F[ValueExpr[Even, Unit]]
  extension (tfunction: F[TypeExpr[Even, Unit]])
    @scala.annotation.targetName("appTEven")
    def apply(nat: F[ValueExpr[Nat, Unit]])(using sp: SourcePos): F[TypeExpr[Even, ValueExpr[Nat, Unit]]]
  extension (ctor: F[ValueExpr[Even, Unit]])
    @scala.annotation.targetName("appVEven")
    def of(args: F[ValueExpr[Nat, Unit]])(using sp: SourcePos): F[ValueExpr[Even, ValueExpr[Nat, Unit]]]
  def ODD: F[TypeExpr[Odd, Unit]]
  def Odd: F[ValueExpr[Odd, Unit]]
  extension (ctor: F[TypeExpr[Odd, Unit]])
    @scala.annotation.targetName("appTOdd")
    def apply(args: F[ValueExpr[Nat, Unit]])(using sp: SourcePos): F[TypeExpr[Odd, ValueExpr[Nat, Unit]]]

object Nats:

  def isEven(v: Term.ValueLevel[Nat, Unit], acc: Int = 0): Int =
    v match
      case dc10.scala.Symbol.Term.ValueLevel.App.App1(qnt, fun, arg, tpe) => ???
      case AppCtor1(qnt, tpe, arg) => isEven(arg.asInstanceOf[Term.ValueLevel[Nat, Unit]], acc + 1)
      case AppCtor2(qnt, tpe, arg1, arg2) => ???
      case AppPure(qnt, fun, arg, tpe) => ???
      case Dot1(qnt, fun, arg1, arg2, tpe) => ???
      case Dotless(qnt, fun, arg1, arg2, tpe) => ???
      case Lam1(qnt, a, b, tpe) => ???
      case Lam2(qnt, a1, a2, c, tpe) => ???
      case BooleanLiteral(qnt, tpe, b) => ???
      case IntLiteral(qnt, tpe, i) => ???
      case StringLiteral(qnt, tpe, s) => ???
      case ListCtor(qnt, tpe) => ???
      case OptionCtor(qnt, tpe) => ???
      case SomeCtor(qnt, tpe) => ???      
      case UserDefinedValue(qnt, nme, tpe, impl) => if nme == "Zero" then acc + 1 else acc
      case _ => ???
    
 
  trait Mixins extends Nats[[A] =>> StateT[ErrorF, List[Statement], A]]:

    def NAT: StateT[ErrorF, List[Statement], TypeExpr[Nat, Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.UserDefinedType(None, "Nat", None, ())))
      
    def Zero: StateT[ErrorF, List[Statement], ValueExpr[Nat, Unit]] =
      for
        t <- NAT
        v <- StateT.pure(ValueExpr(Term.ValueLevel.Var.UserDefinedValue(None, "Zero", t.tpe, None)))
      yield v

    def Succ: StateT[ErrorF, List[Statement], ValueExpr[Nat, Unit]] =
      for
        t <- NAT
        v <- StateT.pure(ValueExpr(Term.ValueLevel.Var.UserDefinedValue(None, "Succ", t.tpe, None)))
      yield v

         
    extension (ctor: StateT[ErrorF, List[Statement], ValueExpr[Nat, Unit]])
      @scala.annotation.targetName("appVSucc")
      def apply(args: StateT[ErrorF, List[Statement], ValueExpr[Nat, Unit]])(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[Nat, Unit]] =
        for
          l <- ctor
          a <- args
          v <- StateT.pure(Term.ValueLevel.App.AppCtor1(None, l.value.tpe, a.value))
        yield ValueExpr(v)

    def EVEN: StateT[ErrorF, List[Statement], TypeExpr[Even, Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.UserDefinedType(None, "Even", None, ())))
      

    def Even: StateT[ErrorF, List[Statement], ValueExpr[Even, Unit]] =
      for
        t <- EVEN
        v <- StateT.pure(ValueExpr(Term.ValueLevel.Var.UserDefinedValue(None, "Even", t.tpe, None)))
      yield v

    extension (tfunction: StateT[ErrorF, List[Statement], TypeExpr[Even, Unit]])
      @scala.annotation.targetName("appTEven")
      def apply(targs: StateT[ErrorF, List[Statement], ValueExpr[Nat, Unit]])(using sp: SourcePos): StateT[ErrorF, List[Statement], TypeExpr[Even, ValueExpr[Nat, Unit]]] =
        for
          f <- tfunction
          a <- targs
          _ <- StateT.liftF[ErrorF, List[Statement], ValueExpr[Nat, Unit]](a.value match
            case dc10.scala.Symbol.Term.ValueLevel.App.App1(qnt, fun, arg, tpe) =>  ???
            case AppCtor1(qnt, tpe, arg) => if !(isEven(arg.asInstanceOf[ValueLevel[Nat, Unit]])%(2) == 1) then Right(a) else Left(List(Error(s"${sp.file}:${sp.line}\nDependent Nat error")))
            case AppCtor2(qnt, tpe, arg1, arg2) => ???
            case AppPure(qnt, fun, arg, tpe) => ???
            case Dot1(qnt, fun, arg1, arg2, tpe) => ???
            case Dotless(qnt, fun, arg1, arg2, tpe) => ???
            case Lam1(qnt, a, b, tpe) => ???
            case Lam2(qnt, a1, a2, c, tpe) => ???
            case BooleanLiteral(qnt, tpe, b) => ???
            case IntLiteral(qnt, tpe, i) => ???
            case StringLiteral(qnt, tpe, s) => ???
            case ListCtor(qnt, tpe) => ???
            case OptionCtor(qnt, tpe) => ???
            case SomeCtor(qnt, tpe) => ???
            case UserDefinedValue(qnt, nme, tpe, impl) => if isEven(a.value)%(2) == 1 then Right(a) else Left(List.empty)          
          )
        yield f.copy(tpe = f.tpe.manageDep(_ => a)

        )
        
    extension (ctor: StateT[ErrorF, List[Statement], ValueExpr[Even, Unit]])
      @scala.annotation.targetName("appVEven")
      def of(args: StateT[ErrorF, List[Statement], ValueExpr[Nat, Unit]])(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[Even, ValueExpr[Nat, Unit]]] =
        for
          l <- ctor
          h <- args
        yield ValueExpr(Term.ValueLevel.App.AppCtor1[Even, Nat, ValueExpr[Nat, Unit]](None, l.value.tpe.manageDep(_ => h), h.value.manageDep(_ => h)))
    
    def ODD: StateT[ErrorF, List[Statement], TypeExpr[Odd, Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.UserDefinedType(None, "Odd", None, ())))
      
    def Odd: StateT[ErrorF, List[Statement], ValueExpr[Odd, Unit]] =
      for
        t <- ODD
        v <- StateT.pure(ValueExpr(Term.ValueLevel.Var.UserDefinedValue(None, "Odd", t.tpe, None)))
      yield v

    extension (tfunction: StateT[ErrorF, List[Statement], TypeExpr[Odd, Unit]])
      @scala.annotation.targetName("appTOdd")
      def apply(targs: StateT[ErrorF, List[Statement], ValueExpr[Nat, Unit]])(using sp: SourcePos): StateT[ErrorF, List[Statement], TypeExpr[Odd, ValueExpr[Nat, Unit]]] =
        for
          f <- tfunction
          a <- targs
    
          _ <- StateT.liftF[ErrorF, List[Statement], ValueExpr[Nat, Unit]](a.value match
            case dc10.scala.Symbol.Term.ValueLevel.App.App1(qnt, fun, arg, tpe) =>  ???
            case AppCtor1(qnt, tpe, arg) =>
              if !(isEven(arg.asInstanceOf[ValueLevel[Nat, Unit]])%(2) == 0) then Right(a) else Left(List(Error(s"${sp.file}:${sp.line}\nDependent Nat apply error")))
            case AppCtor2(qnt, tpe, arg1, arg2) => ???
            case AppPure(qnt, fun, arg, tpe) => ???
            case Dot1(qnt, fun, arg1, arg2, tpe) => ???
            case Dotless(qnt, fun, arg1, arg2, tpe) => ???
            case Lam1(qnt, a, b, tpe) => ???
            case Lam2(qnt, a1, a2, c, tpe) => ???
            case BooleanLiteral(qnt, tpe, b) => ???
            case IntLiteral(qnt, tpe, i) => ???
            case StringLiteral(qnt, tpe, s) => ???
            case ListCtor(qnt, tpe) => ???
            case OptionCtor(qnt, tpe) => ???
            case SomeCtor(qnt, tpe) => ???
            case UserDefinedValue(qnt, nme, tpe, impl) =>
              if isEven(a.value)%(2) == 0 then Right(a) else Left(List(Error(s"${sp.file}:${sp.line}\nDependent Nat value error")))
          )
          
        yield f.copy(tpe = f.tpe.manageDep(_ => a)

        )