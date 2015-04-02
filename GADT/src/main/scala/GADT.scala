// Benedict R. Gaster
// March 2015
//
// GADTs in Scala
//
// Of course, many will argue these are in fact inductive familes
// and clearly Haskell's GADTs came later but as I started there for
// this particular exercise I'll stick with it...
//
// I make no claim for any of this being original or new, rather I
// wanted to develop my understanding of implementing GADTs in Scala.
// Still early days in that regard :-)

//------------------------------------------------------------------------------------

// Use the Leibniz library from scalaz to implement type level equality
// see: https://github.com/scalaz/scalaz

// Depends on the compile plugin kind-projector, so we get nice type
// level lambdas see: https://github.com/non/kind-projector

//------------------------------------------------------------------------------------

// Of course, we begin with the standard correctly typed expression
// evaluator, and extend it to simply typed lambda calculus afterwards.
// As we parametrize only a single type there is no issue with
// type refinement in this case
package object Exp {
  sealed trait Exp[A] {
    def eval: A = Exp.evalAny(this)
  }

  case class LitInt(i: Int)                                     extends Exp[Int]
  case class LitBool(b: Boolean)                                extends Exp[Boolean]
  case class Add(e1: Exp[Int], e2: Exp[Int])                    extends Exp[Int]
  case class Mul(e1: Exp[Int], e2: Exp[Int])                    extends Exp[Int]
  case class Cond[A](b: Exp[Boolean], thn: Exp[A], els: Exp[A]) extends Exp[A]
  case class Eq[A](e1: Exp[A], e2: Exp[A])                      extends Exp[Boolean]

  object Exp {
    def evalAny[A](e: Exp[A]): A = e match {
      case LitInt(i) => i
      case LitBool(b) => b
      case Add(e1, e2) => e1.eval + e2.eval
      case Mul(e1, e2) => e1.eval * e2.eval
      case Cond(b, thn, els) => if (b.eval) { thn.eval } else { els.eval }
      case Eq(e1, e2) => e1.eval == e2.eval
    }
  }

  val exp10 = LitInt(10)
  val exp20 = LitInt(20)
  val exp30 = Add(exp10,exp20)
  val expF  = LitBool(false)
  val v30   = exp30.eval
  // next one won't compile, of course...
  //  val expFp20 = Add(expF, exp20)
}

package object STLam {
  sealed trait STLam[A] {
    def eval: A = STLam.evalAny(this)
  }

  case class Lift[A] ( a: A ) extends STLam[A]
  case class Tup[A,B] ( fst: STLam[A], snd: STLam[B] ) extends STLam[(A,B)]
  case class Lam[A,B]( f: STLam[A] => STLam[B]) extends STLam[A => B]
  case class App[A,B]( f: STLam[A => B], a : STLam[A]) extends STLam[B]
  case class Fix[A,B] ( f: STLam[(A=>B) => (A=>B)] ) extends STLam[A=>B]

  object STLam {
    // fixed point operator
    // (http://rosettacode.org/wiki/Y_combinator#Scala)
    private def Y[A,B](f: (A=>B)=>(A=>B)) = {
      case class W(wf: W=>A=>B) {
        def apply(w: W) = wf(w)
      }
      val g: W=>A=>B = w => f(w(w))(_)
      g(W(g))
    }

    def evalAny[A](l: STLam[A]): A = l match {
      case Lift(a)    => a
      case Tup(e1,e2) => (e1.eval, e2.eval)
      case Lam(f)     => (x) => f(Lift(x)).eval
      case App(f,e)   => f.eval (e.eval)
      case Fix(f)     => Y(f.eval)
        // note as Scala is strict we can't do a 1:1 mapping with the
        // Haskell version, i.e. we can't implement fized point
        // operator inline. at least i don't know how to do it.
    }
  }

  val id = Lam[Int,Int] (x => x)
  val ten = App(id, Lift(10)).eval

  // factorial 
  val fact = (x: Int) =>
    App(Fix[Int,Int] (
    Lam (f =>
      Lam (v => {
        val n = v.eval
        Lift( if (n == 0)
          1
        else
          n * f.eval (n - 1)) }))), Lift(x)).eval

  // fibonacci
  val fib = (x: Int) =>
    App(Fix[Int,Int] (
      Lam (f =>
        Lam (v => {
          val n = v.eval
          Lift( if (n < 3)
            n
          else
            f.eval(n - 1) + f.eval (n - 2)) }))), Lift(x)).eval
}

//------------------------------------------------------------------------------------

// Implementation of SafeList from Haskell's wiki on GADTs:
// http://en.wikibooks.org/wiki/Haskell/GADT#Examples This is a
// example of usful technique of using "empty" types to index
// into a type.

// We implement two variants, as per the wiki. The first also
// implements the excerise (safeTail) and so is slightly extended
// compared to the version documented on the wiki. Note to get a
// generic safeTail, as can be seen in the implementation, I had
// to change the type slightly.

// Of course, this isomorphic lists with a length as are effectively
// encoding this in the type.
package object SafeList {
//  import shapeless._
//  import syntax.singleton._

  final case class Empty()
  final case class NonEmpty[A]() // A represents the rest of the list,
                                 // needed for safeTail

  sealed trait SafeList[+A,B] 
  final case class SNil[+A]() extends SafeList[A,Empty]
  final case class SCons[+A,B]( a : A, sl : SafeList[A,B] ) extends SafeList[A,NonEmpty[B]]

  def safeHead[A,B]( sl: SafeList[A,NonEmpty[B]] ) : A = sl match {
    case SCons(h,_) => h
    // no warning for not matching SNil, even though it a sealed class...
  }

  def safeTail[A,B]( sl : SafeList[A,NonEmpty[B]] ) : SafeList[A,B] = sl match {
    case SCons(_,tl) => tl
      // no warning for not matching SNil, even though it a sealed class...
  }

  // Some trivial examples...
  val nonempty = SCons(10, SCons(20, SNil()))
  val empty : SafeList[Int,Empty] = SNil[Int]()

  // scala> safeHead(nonempty)
  // res1: Int = 10

  // next one does not work, giving the expected type error
  // scala> safeHead(empty)
  // <console>:11: error: type mismatch;
  // found   : SafeList.SafeList[Int,SafeList.Empty]
  // required: SafeList.SafeList[?,SafeList.NonEmpty]
  //            safeHead(empty)

  // Of course, just like the Haskell implementation we can't implement
  // a function silly that is dependent on a boolean value to construct
  // either a SNill or SCons list and retain the type information.
  final case class NotSafe()
  final case class Safe()
  sealed trait MarkedList[+A,+B]
  final case class MNil[+A]() extends MarkedList[A,NotSafe] 
  final case class MCons[+A,+B,+C]( a : A, sl : MarkedList[A,B] ) extends MarkedList[A,C]

  val mnonempty = MCons(10, MCons(20, MNil()))

  def safeHead[A]( sl: MarkedList[A,Safe] ) : A = sl match {
    case MCons(h,_) => h
    // no warning for not matching SNil, even though it a sealed class...
  }

  // Now we can write a function that produces something that can
  // never be consumed by safeHead!!
  def silly(b : Boolean) : MarkedList[Unit,NotSafe] = b match {
    case false => MNil()
    case true => MCons((),MNil())
  }
}

// Classic dependent type sized lists (aka vectors) based on the ideas
// in the excellent talk:
//   Scala vs. Idris: Dependent types now and in the future" at Strange Loop 2013
//   http://www.infoq.com/presentations/scala-idris
package object Vector {
  // Define some natural numbers
  sealed trait Nat
  trait Z extends Nat
  case object Z extends Z
  case class S[N <: Nat](n: N) extends Nat

  sealed trait Vector[+A, N <: Nat]
  case class ZV[+A]() extends Vector[A,Z]

  implicit class VPlus[A, N <: Nat](xs: Vector[A, N]) {
    def ++[M <: Nat](ys: Vector[A, M])
      (implicit plus: Plus[A, Vector[A, N], Vector[A, M]]): plus.Out = plus(xs, ys)

  }

  trait Plus[A, XS, YS] {
    type Result <: Nat
    type Out = Vector[A, Result]
    def apply(xs: XS, ys: YS): Out
  }

  // Define plus (concat) for our vectors with our natural numbers
  object Plus {
    implicit def plusZ[A, N <: Nat] = new Plus[A, Vector[A,Z], Vector[A,N]] {
      type Result = N
      def apply(xs: Vector[A,Z], ys: Vector[A,N]) = ys
    }

    implicit def plusS[A, N <: Nat, M <: Nat]
      (implicit plus: Plus[A, Vector[A, N], Vector[A,M]]) =
         new Plus[A, Vector[A, S[N]], Vector[A,M]] {
           type Result = S[plus.Result]
           def apply(xs: Vector[A, S[N]], ys: Vector[A,M]): Out = SV(vhead(xs), (vtail(xs) ++ ys))
    }
  }

  case class SV[+A, N <: Nat]( a: A, v: Vector[A,N] ) extends Vector[A, S[N]]

  def vhead[A,N <: Nat]( v : Vector[A,S[N]] ) : A = v match {
    case SV(a,_) => a
  }

  def vtail[A,N <: Nat]( v: Vector[A,S[N]] ) : Vector[A, N] = v match {
    case SV(_,v) => v 
  }

  def vconcat[A, N <: Nat, M <: Nat](xs: Vector[A, N], ys: Vector[A, M])
    (implicit plus: Plus[A, Vector[A, N], Vector[A, M]]): plus.Out = xs ++ ys
}

package object BasicRefinement {
  import scalaz._
  import Leibniz._

  // The following example is based a comment from the blog:
  // http://pchiusano.blogspot.co.uk/2010/06/gadts-in-scala.html An
  // example is given that does not work in Scala but does in Haskell.
  //
  // The solution to handle type refinement is based on the approach blogged here:
  //   http://d.hatena.ne.jp/xuwei/20140706/1404612620
  sealed trait Eq[A,B] {
    def cata[Z](x: (A === B) => Z): Z
  }

  sealed case class Refl[A]() extends Eq[A,A] {
    def cata[Z](x: (A === A) => Z) =
      x(Leibniz.refl)
  }

  def eval[A](a : A, eq : Eq[A,Int]) : Int =
    eq.cata(x => x.subst[λ[b => b]](a)) + 1 

  val ex = eval(1, Refl())

  // scala> eval(1, Refl())
  // res1: Int = 2

  // next one does not work, giving the expected type error
  // scala> val exS = eval('a', Refl())
  // <console>:10: error: type mismatch;
  // found   : Refinement.Refl[Int]
  // required: Refinement.Eq[AnyVal,Int]
  // Note: Int <: AnyVal (and Refinement.Refl[Int] <: Refinement.Eq[Int,Int]), but trait Eq is invariant in type A.
  // You may wish to define A as +A instead. (SLS 4.5)
}

//----------------------------------------------------------------------------------

package object Foo {
  import scalaz._
  import Leibniz._

  // The following is a variant on BasicRefinement example, above. It
  // is a slight differece to the example approach blogged here:
  // http://d.hatena.ne.jp/xuwei/20140706/1404612620

  sealed abstract class Foo[A, B] {
    def cata[Z](x: (A === B) => Z, y: (A, B) => Z): Z
  }

  final case class X[A]() extends Foo[A, A] {
    def cata[Z](x: (A === A) => Z, y: (A, A) => Z) =
      x(Leibniz.refl)
  }

  final case class Y[A, B](a: A, b: B) extends Foo[A, B] {
    def cata[Z](x: (A === B) => Z, y: (A, B) => Z) =
      y(a, b)
  }

  def hoge[F[_,_], A, B, C](foo: Foo[A, B], bar: F[A, C]): F[B, C] =
    foo.cata(x => x.subst[λ[a => F[a,C]]](bar), 
             (_, _) => sys error "nonexhaustive")
}
