package com.github.dlomsak.regex.deriv
import matryoshka._
import matryoshka.data.Fix
import matryoshka.implicits._

import shapeless._
import Coproduct._
import shapeless.{:+: => cpr}
import matryoshka.patterns._
import scalaz._
import Scalaz._
sealed trait RegexASTF[A] {

}

final case class CharASTF[A](c: Char) extends RegexASTF[A]
final case class NullASTF[A]() extends RegexASTF[A]
final case class EmptyASTF[A]() extends RegexASTF[A]
final case class OrASTF[A](l: A, r: A) extends RegexASTF[A]
final case class CatASTF[A](l: A, r: A) extends RegexASTF[A]
final case class StarASTF[A](r: A) extends RegexASTF[A]

object RegexASTF {
  def coalgebra: Coalgebra[RegexASTF, RegexAST] = {
    case CharAST(c) => CharASTF(c)
    case NullAST => NullASTF()
    case EmptyAST => EmptyASTF()
    case OrAST(l, r) => OrASTF(l, r)
    case CatAST(l, r) => CatASTF(l, r)
    case StarAST(r) => StarASTF(r)
  }

  def algebra: Algebra[RegexASTF, RegexAST] = {
    case CharASTF(c) => CharAST(c)
    case NullASTF() => NullAST
    case EmptyASTF() => EmptyAST
    case OrASTF(l, r) => OrAST(l, r)
    case CatASTF(l, r) => CatAST(l, r)
    case StarASTF(r) => StarAST(r)
  }

  implicit val traverse: scalaz.Traverse[RegexASTF] = new scalaz.Traverse[RegexASTF] {
    override def traverseImpl[G[_], A, B]
    (fa: RegexASTF[A])(f: (A) => G[B])
    (implicit G: Applicative[G]): G[RegexASTF[B]] = fa match {
      case CharASTF(c) => G.point(CharASTF(c))
      case NullASTF() => G.point(NullASTF())
      case EmptyASTF() => G.point(EmptyASTF())
      case OrASTF(l, r) => (f(l) |@| f(r))(OrASTF(_, _))
      case CatASTF(l, r) => (f(l) |@| f(r))(CatASTF(_, _))
      case StarASTF(r) => f(r) map (StarASTF(_))

    }
  }

  implicit val birecursive = Birecursive.algebraIso(algebra, coalgebra)

  def affix: Algebra[RegexASTF, Fix[RegexASTF]] = {
    case x => Fix(x)
  }



  def depth: (Int, Fix[RegexASTF]) => EnvT[Int, RegexASTF, Fix[RegexASTF]] = {
      case (i, Fix(c @ CharASTF('c'))) => EnvT((i + 1, c))
    }

  val fixed = birecursive.cata(CharAST('c'))(affix)








  def depth[T, F[_]: Functor](implicit T: Recursive.Aux[T, F])
  : Coalgebra[EnvT[Int, F, ?], (Int, T)] = {
    case (i, t) => EnvT(i, t.project strengthL (i + 1))
  }




























}