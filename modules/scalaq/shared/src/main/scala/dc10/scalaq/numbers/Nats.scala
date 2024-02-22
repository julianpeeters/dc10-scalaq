package dc10.scalaq.numbers

import cats.data.StateT
import cats.implicits.given
// import dc10.scala.dsl.{==>}
import dc10.scala.{Error, ErrorF, Statement}
// import dc10.scala.{ErrorF, Statement}
import dc10.scala.Statement.{TypeExpr, ValueExpr}
import dc10.scala.Symbol.Term
import org.tpolecat.sourcepos.SourcePos
import dc10.scala.Symbol.Term.TypeLevel.Var.UserDefinedType
import dc10.scala.Symbol.Term.ValueLevel.App.AppCtor1
import dc10.scala.Symbol.Term.ValueLevel.App.AppCtor2
import dc10.scala.Symbol.Term.ValueLevel.App.AppPure
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
import dc10.scala.Symbol.Term.ValueLevel
// import dc10.scala.Symbol.Term.ValueLevel.App.AppVargs

type Nat
type Even
type Odd
type Even2

trait Nats[F[_]]:
  def NAT: F[TypeExpr[Nat, Unit]]
  def Zero: F[ValueExpr[Nat, Unit]]
  def Succ: F[ValueExpr[Nat, Unit]]
  def EVEN: F[TypeExpr[Even, Unit]]
  def Even: F[ValueExpr[Even, Unit]]
  extension [Z] (tfunction: F[TypeExpr[Even, Z]])
    @scala.annotation.targetName("appTEven")
    def apply(nat: F[ValueExpr[Nat, Unit]])(using sp: SourcePos): F[TypeExpr[Even, Nat]]
  // extension (ctor: F[ValueExpr[Even, Unit]])
  //   @scala.annotation.targetName("appVEven")
  //   def of(args: F[ValueExpr[Nat, Unit]])(using sp: SourcePos): F[ValueExpr[Even, ValueExpr[Nat, Unit]]]

  def ODD: F[TypeExpr[Odd, Unit]]
  def Odd: F[ValueExpr[Odd, Unit]]
  extension [Z] (ctor: F[TypeExpr[Odd, Z]])
    @scala.annotation.targetName("appTOdd")
    def apply(args: F[ValueExpr[Nat, Unit]])(using sp: SourcePos): F[TypeExpr[Odd, Nat]]

  def EVEN2: F[TypeExpr[Even2, Unit]]
  def Even2: F[ValueExpr[Even2, Unit]]
  extension [Z] (tfunction: F[TypeExpr[Even2, Z]])
    @scala.annotation.targetName("appTEven2")
    def apply(nat: F[ValueExpr[Nat, Unit]])(using sp: SourcePos): F[TypeExpr[Even2, Nat]]
  // extension (ctor: F[ValueExpr[Even2, Unit]])
  //   @scala.annotation.targetName("appVEven2")
  //   def of(args: F[ValueExpr[Nat, Unit]])(using sp: SourcePos): F[ValueExpr[Even2, ValueExpr[Nat, Unit]]]

  // def EVENZ: F[TypeExpr[Even, ValueExpr[Nat, Unit]]]
  // def EVENS(n: F[ValueExpr[Nat, Unit]]): F[TypeExpr[Even => Even, Unit]] 
  // def EVEN2Z: F[TypeExpr[Even2, ValueExpr[Nat, Unit]]]
  // def EVEN2S(n: F[ValueExpr[Nat, Unit]]): F[TypeExpr[Odd => Even2, Unit]] 

  // def EvenImpliesEven2(n: F[ValueExpr[Nat, Unit]])(even: Even, even2: Even): F[TypeExpr[Even => Even2, Unit]]

  // def EvenImpliesEven2Z: F[TypeExpr[Even, ValueExpr[Nat, Unit]]]

object Nats:

  def isEven(v: Term.ValueLevel[Nat, Unit], acc: Int = 0): Int =
    v match
      case dc10.scala.Symbol.Term.ValueLevel.App.App1(qnt, fun, arg, tpe) => ???
      case AppCtor1(qnt, tpe, arg) => isEven(arg.asInstanceOf[Term.ValueLevel[Nat, Unit]], acc + 1)
      case AppCtor2(qnt, nme, tpe, arg1, arg2) => ???
      case AppPure(qnt, fun, arg, tpe) => ???
      // case AppVargs(qnt, fun, arg, a*) => ???
      case ValueLevel.Blc.ForComp(qnt, gens, tpe, z) => ???
      case Dot1(qnt, fun, arg1, arg2, tpe) => ???
      case Dotless(qnt, fun, arg1, arg2, tpe) => ???
      case Lam1(qnt, a, b, tpe) => ???
      case Lam2(qnt, a1, a2, c, tpe) => ???
      case BooleanLiteral(qnt, tpe, b) => ???
      case IntLiteral(qnt, tpe, i) => ???
      case StringLiteral(qnt, tpe, s) => ???
      case UnitLiteral(qnt, tpe, s) => ???
      case ListCtor(qnt, tpe) => ???
      case OptionCtor(qnt, tpe) => ???
      case SomeCtor(qnt, tpe) => ???      
      case TupleCtor(qnt, tpe) => ???      
      case UserDefinedValue(qnt, nme, tpe, impl) => if nme == "Zero" then acc + 1 else acc
    
 
  trait Mixins extends Nats[[A] =>> StateT[ErrorF, List[Statement], A]]:

    def NAT: StateT[ErrorF, List[Statement], TypeExpr[Nat, Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.UserDefinedType(None, "Nat", None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()))))
      
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
      StateT.pure(TypeExpr(Term.TypeLevel.Var.UserDefinedType(None, "Even", None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()))))

    def Even: StateT[ErrorF, List[Statement], ValueExpr[Even, Unit]] =
      for
        t <- EVEN
        v <- StateT.pure(ValueExpr(Term.ValueLevel.Var.UserDefinedValue(None, "Even", t.tpe, None)))
      yield v

    extension [Z] (tfunction: StateT[ErrorF, List[Statement], TypeExpr[Even, Z]])
      @scala.annotation.targetName("appTEven")
      def apply(targs: StateT[ErrorF, List[Statement], ValueExpr[Nat, Unit]])(using sp: SourcePos): StateT[ErrorF, List[Statement], TypeExpr[Even, Nat]] =
        for
          f <- tfunction
          a <- targs
          _ <- StateT.liftF[ErrorF, List[Statement], ValueExpr[Nat, Unit]](a.value match
            case dc10.scala.Symbol.Term.ValueLevel.App.App1(qnt, fun, arg, tpe) =>  ???
            case AppCtor1(qnt, tpe, arg) => if !(isEven(arg.asInstanceOf[ValueLevel[Nat, Unit]])%(2) == 1) then Right(a) else Left(List(Error(s"${sp.file}:${sp.line}\nDependent Nat error")))
            case AppCtor2(qnt, nme, tpe, arg1, arg2) => ???
            case AppPure(qnt, fun, arg, tpe) => ???
            case ValueLevel.Blc.ForComp(qnt, gens, tpe, z) => ???
            case Dot1(qnt, fun, arg1, arg2, tpe) => ???
            case Dotless(qnt, fun, arg1, arg2, tpe) => ???
            case Lam1(qnt, a, b, tpe) => ???
            case Lam2(qnt, a1, a2, c, tpe) => ???
            case BooleanLiteral(qnt, tpe, b) => ???
            case IntLiteral(qnt, tpe, i) => ???
            case StringLiteral(qnt, tpe, s) => ???
            case UnitLiteral(qnt, tpe, s) => ???
            case ListCtor(qnt, tpe) => ???
            case OptionCtor(qnt, tpe) => ???
            case SomeCtor(qnt, tpe) => ???
            case TupleCtor(qnt, tpe) => ???
            case UserDefinedValue(qnt, nme, tpe, impl) => impl.fold(Right(a))(i => if Nats.isEven(i)%(2) == 1 then Right(a) else Left(List(Error(s"${sp.file}:${sp.line}\nDependent Even nat value error"))))
          )

        yield f.copy(tpe = f.tpe.manageDep(_ => a.value.asInstanceOf[ValueLevel[Nat, Any]])

        )
        
    // extension (ctor: StateT[ErrorF, List[Statement], ValueExpr[Even, Unit]])
    //   @scala.annotation.targetName("appVEven")
    //   def of(args: StateT[ErrorF, List[Statement], ValueExpr[Nat, Unit]])(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[Even, ValueExpr[Nat, Unit]]] =
    //     for
    //       l <- ctor
    //       h <- args
    //     yield ValueExpr(Term.ValueLevel.App.AppCtor1[Even, Nat, ValueExpr[Nat, Unit]](None, l.value.tpe.manageDep(_ => h), h.value.manageDep(_ => h)))
    
    def ODD: StateT[ErrorF, List[Statement], TypeExpr[Odd, Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.UserDefinedType(None, "Odd", None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()))))
      
    def Odd: StateT[ErrorF, List[Statement], ValueExpr[Odd, Unit]] =
      for
        t <- ODD
        v <- StateT.pure(ValueExpr(Term.ValueLevel.Var.UserDefinedValue(None, "Odd", t.tpe, None)))
      yield v

    extension [Z] (tfunction: StateT[ErrorF, List[Statement], TypeExpr[Odd, Z]])
      @scala.annotation.targetName("appTOdd")
      def apply(targs: StateT[ErrorF, List[Statement], ValueExpr[Nat, Unit]])(using sp: SourcePos): StateT[ErrorF, List[Statement], TypeExpr[Odd, Nat]] =
        for
          f <- tfunction
          a <- targs
    
          _ <- StateT.liftF[ErrorF, List[Statement], ValueExpr[Nat, Unit]](a.value match
            case dc10.scala.Symbol.Term.ValueLevel.App.App1(qnt, fun, arg, tpe) =>  ???
            case AppCtor1(qnt, tpe, arg) =>
              if !(isEven(arg.asInstanceOf[ValueLevel[Nat, Unit]])%(2) == 0) then Right(a) else Left(List(Error(s"${sp.file}:${sp.line}\nDependent Nat apply error")))
            case AppCtor2(qnt, nme, tpe, arg1, arg2) => ???
            case AppPure(qnt, fun, arg, tpe) => ???
            case ValueLevel.Blc.ForComp(qnt, gens, tpe, z) => ???
            case Dot1(qnt, fun, arg1, arg2, tpe) => ???
            case Dotless(qnt, fun, arg1, arg2, tpe) => ???
            case Lam1(qnt, a, b, tpe) => ???
            case Lam2(qnt, a1, a2, c, tpe) => ???
            case BooleanLiteral(qnt, tpe, b) => ???
            case IntLiteral(qnt, tpe, i) => ???
            case StringLiteral(qnt, tpe, s) => ???
            case UnitLiteral(qnt, tpe, u) => ???
            case ListCtor(qnt, tpe) => ???
            case OptionCtor(qnt, tpe) => ???
            case SomeCtor(qnt, tpe) => ???
            case TupleCtor(qnt, tpe) => ???
            case UserDefinedValue(qnt, nme, tpe, impl) =>
              if isEven(a.value)%(2) == 0 then Right(a) else Left(List(Error(s"${sp.file}:${sp.line}\nDependent Nat value error")))
          )
          
        yield f.copy(tpe = f.tpe.manageDep(_ => a.value.asInstanceOf[ValueLevel[Nat, Any]])

        )


    def EVEN2: StateT[ErrorF, List[Statement], TypeExpr[Even2, Unit]] =
      StateT.pure(TypeExpr(Term.TypeLevel.Var.UserDefinedType(None, "Even2", None, Term.ValueLevel.Var.UnitLiteral(None, Term.TypeLevel.Var.UnitType(None), ()))))

    def Even2: StateT[ErrorF, List[Statement], ValueExpr[Even2, Unit]] =
      for
        t <- EVEN2
        v <- StateT.pure(ValueExpr(Term.ValueLevel.Var.UserDefinedValue(None, "Even2", t.tpe, None)))
      yield v

    extension [Z] (tfunction: StateT[ErrorF, List[Statement], TypeExpr[Even2, Z]])
      @scala.annotation.targetName("appTEven2")
      def apply(targs: StateT[ErrorF, List[Statement], ValueExpr[Nat, Unit]])(using sp: SourcePos): StateT[ErrorF, List[Statement], TypeExpr[Even2, Nat]] =
        for
          f <- tfunction
          a <- targs
          _ <- StateT.liftF[ErrorF, List[Statement], ValueExpr[Nat, Unit]](a.value match
            case dc10.scala.Symbol.Term.ValueLevel.App.App1(qnt, fun, arg, tpe) =>  ???
            case AppCtor1(qnt, tpe, arg) => if !(Nats.isEven(arg.asInstanceOf[ValueLevel[Nat, Unit]])%(2) == 1) then Right(a) else Left(List(Error(s"${sp.file}:${sp.line}\nDependent Nat error")))
            case AppCtor2(qnt, nme, tpe, arg1, arg2) => ???
            case AppPure(qnt, fun, arg, tpe) => ???
            case ValueLevel.Blc.ForComp(qnt, gens, tpe, z) => ???
            case Dot1(qnt, fun, arg1, arg2, tpe) => ???
            case Dotless(qnt, fun, arg1, arg2, tpe) => ???
            case Lam1(qnt, a, b, tpe) => ???
            case Lam2(qnt, a1, a2, c, tpe) => ???
            case BooleanLiteral(qnt, tpe, b) => ???
            case IntLiteral(qnt, tpe, i) => ???
            case StringLiteral(qnt, tpe, s) => ???
            case UnitLiteral(qnt, tpe, u) => ???
            case ListCtor(qnt, tpe) => ???
            case OptionCtor(qnt, tpe) => ???
            case SomeCtor(qnt, tpe) => ???
            case TupleCtor(qnt, tpe) => ???
            case UserDefinedValue(qnt, nme, tpe, impl) =>
              impl.fold(Right(a))(i => if Nats.isEven(i)%(2) == 1 then Right(a) else Left(List(Error(s"${sp.file}:${sp.line}\nDependent Nat value error"))))
          )
        yield f.copy(tpe = f.tpe.manageDep(_ => a.value.asInstanceOf[ValueLevel[Nat, Any]])

        )
        
    // extension (ctor: StateT[ErrorF, List[Statement], ValueExpr[Even2, Unit]])
    //   @scala.annotation.targetName("appVEven2")
    //   def of(args: StateT[ErrorF, List[Statement], ValueExpr[Nat, Unit]])(using sp: SourcePos): StateT[ErrorF, List[Statement], ValueExpr[Even2, ValueExpr[Nat, Unit]]] =
    //     for
    //       l <- ctor
    //       h <- args
    //     yield ValueExpr(Term.ValueLevel.App.AppCtor1[Even2, Nat, ValueExpr[Nat, Unit]](None, l.value.tpe.manageDep(_ => h), h.value.manageDep(_ => h)))

    // def EVENZ: StateT[ErrorF, List[Statement], TypeExpr[Even, ValueExpr[Nat, Unit]]] =
    //   EVEN(Zero)

    // def EVENS(n: StateT[ErrorF, List[Statement], ValueExpr[Nat, Unit]]): StateT[ErrorF, List[Statement], TypeExpr[Even => Even, Unit]] =
    //   EVEN(n) ==> EVEN(Succ(Succ(n)))

    // def EVEN2Z: StateT[ErrorF, List[Statement], TypeExpr[Even2, ValueExpr[Nat, Unit]]] =
    //   EVEN2(Zero)

    // def EVEN2S(n: StateT[ErrorF, List[Statement], ValueExpr[Nat, Unit]]): StateT[ErrorF, List[Statement], TypeExpr[Odd => Even2, Unit]] =
    //   ODD(n) ==> EVEN2(Succ(n))
    
    // def EvenImpliesEven2(
    //   n: StateT[ErrorF, List[Statement], ValueExpr[Nat, Unit]]
    // ): StateT[ErrorF, List[Statement], TypeExpr[Even => Even2, Unit]] =
    //   EVEN(n) ==> EVEN2(n)