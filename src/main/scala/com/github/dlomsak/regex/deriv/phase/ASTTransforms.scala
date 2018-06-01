package com.github.dlomsak.regex.deriv.phase

import com.github.dlomsak.regex.deriv.{CharAST, RegexASTF}
import com.github.dlomsak.regex.deriv.RegexASTF.{affix, birecursive, depth}
import matryoshka.patterns._
import matryoshka._
import matryoshka.implicits._
import com.github.dlomsak.regex.deriv._

import scalaz._
import Scalaz._
import scalaz.Alpha.F
object ASTTransforms {

  val fixed = birecursive.cata(CharAST('c'))(affix)



  import matryoshka.data._

  type ASTFIntTuple[A] = (Int, RegexASTF[A])



  type RegexASTFF = RegexASTF[Fix[RegexASTF]]


  def depth3: Coalgebra[EnvT[Int, RegexASTF, ?], Fix[RegexASTF]] = {
    case a @ Fix(CharASTF('c')) => EnvT(1, a.unFix)
  }

  def depth2[F[_]: Functor]: Algebra[F, Int => Cofree[F, Int]] =
    f => i => Cofree(i, f.map(_(i + 1)))


  def depth34(implicit F: Functor[RegexASTF], T: Recursive.Aux[Fix[RegexASTF], RegexASTF])
  : Coalgebra[EnvT[Int, RegexASTF, ?], (Int, Fix[RegexASTF])] = {
    case (i, Fix(CharASTF(c))) => EnvT((i, CharASTF(c)))
  }

  case class MinMaxDepth(min: Int, max: Option[Int]) {
    def incrMin(i: Int = 1) = this.copy(min=this.min + i)

    def incrMax(i: Int = 1) = this.copy(max=this.max.map(_ + i))

    def incrBoth(i: Int = 1) = this.copy(min=this.min+i, max=this.max.map(_ + i))

    def incrBoth(min: Int, max: Int) = this.copy(min=this.min + min, max=this.max.map(_ + max))

    def noMax = this.copy(max=None)

    def +(that: MinMaxDepth) = MinMaxDepth(this.min + that.min, this.max.flatMap {m => that.max.map{_ + m}})
  }

  def depth5(implicit F: Functor[RegexASTF], T: Recursive.Aux[RegexAST, RegexASTF])
  : Coalgebra[EnvT[Int, RegexASTF, ?], (Int, RegexAST)] = {
    case (i, CharAST(c)) => EnvT((i + 1, CharASTF(c)))
    case (i, CatAST(l, r)) => EnvT((i, CatASTF((i, l), (i, r))))
  }

  def annotateMinMax(implicit F: Functor[RegexASTF])
  : Coalgebra[EnvT[MinMaxDepth, RegexASTF, ?], (MinMaxDepth, RegexAST)] = {
    case (i, CharAST(c)) => EnvT((i.incrBoth(), CharASTF(c)))
    case (i, CatAST(l, r)) => EnvT((i, CatASTF((i, l), (i, r))))
    case (i, OrAST(l, r)) => EnvT((i, OrASTF((i, l), (i,r))))

  }

  def depthAlgebra: Algebra[RegexASTF, EnvT[Int, Id, RegexAST]] = {
    case CharASTF(c) => EnvT[Int, Id, RegexAST]((0, CharAST(c)))
    case CatASTF(EnvT((i, r)), EnvT((i2, r2))) => EnvT[Int, Id, RegexAST]((i + i2, CatAST(r, r2)))
    case OrASTF(EnvT((i, r)), EnvT((i2, r2))) => EnvT[Int, Id, RegexAST]((i + i2, OrAST(r, r2)))
  }

  val r1 = REParser(RELexer("this (could be|is) a regex").right.get).right.get

  birecursive.cata(r1)(depthAlgebra)



  def distFromTop(implicit F: Functor[RegexASTF], T: Recursive.Aux[RegexAST, RegexASTF])
  :Algebra[EnvT[MinMaxDepth, RegexASTF, ?], EnvT[MinMaxDepth, RegexASTF, RegexAST]] = {
    case EnvT((minMax, d @ CharASTF(c))) => EnvT((minMax, CharASTF[RegexAST](c)))
  }





  trait CS[A]
  case class CharSeqAST[A](s: String) extends CS[A]

  type ASTFAndSeq[A] =scalaz.Coproduct[RegexASTF, CharSeqAST, A]


  case class MyInject[F[_], G[_]](i: Inject[F, G]) {
    def unapply[A](g: G[A]): Option[F[A]] = i.prj(g)

  }


  implicit class Coproductable[F[_], A, G[_]](a: F[A])(implicit I: Inject[F, G]) {
    def unapply(g: G[A]) = I.prj(g)
  }

  implicit val csInj = scalaz.Inject[CharSeqAST, ASTFAndSeq]

  implicit val InjectCharSeqAST = new MyInject(scalaz.Inject[CharSeqAST, ASTFAndSeq])

  val res = InjectCharSeqAST.i(CharSeqAST("hello"))




  type ASTFCFix[F[_]] = ASTFAndSeq[Fix[F]]
  CharSeqAST[Int]("asdf")

  type ij[F[_], A] = Inject[ASTFAndSeq, F]

  implicit class IJ[F[_], G[_]](I: Inject[F, G]) {
    def unapply[A](f: G[A]): Option[F[A]] = I.prj(f)
  }

  def ff[F[_]](implicit I: Inject[CharSeqAST, F]): CharSeqAST[Fix[F]] => F[Fix[F]] = {
    case CharSeqAST(s) => I(CharSeqAST(s))
  }




  val r = REParser(RELexer("this (could be|is) a* a regex").right.get).right.get




  def histo: GAlgebra[Cofree[RegexASTF, ?], RegexASTF, (MinMaxDepth, RegexAST)] = {
    case CharASTF(c) => (MinMaxDepth(1, Some(1)), CharAST(c))
    case CatASTF(Cofree((mm1, r), t), Cofree((mm2, r2), t2)) => (mm1 + mm2, CatAST(r, r2))
    case StarASTF(Cofree((mm, r), t)) => (MinMaxDepth(mm.min -1, None), r)
    case OrASTF(Cofree((mm1, r), t), Cofree((mm2, r2), t2)) =>
      (MinMaxDepth(mm1.min.min(mm2.min), mm2.max.flatMap {m1 => mm1.max.map{_.max(m1)}}), OrAST(r, r2))
  }





  println(birecursive.histo(r)(histo))


//  def futu[A]
//  (a: A)
//  (f: GCoalgebra[Free[Base, ?], Base, A])
//  (implicit BF: Functor[Base])
//  : T =
//    gana[Free[Base, ?], A](a)(distFutu, f)





  type envt[A] = EnvT[Int, RegexASTF, A]

  (0, fixed).ana[Cofree[RegexASTF, Int]][EnvT[Int, RegexASTF, ?]](depth34)



}
