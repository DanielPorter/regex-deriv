package com.github.dlomsak.regex.deriv.phase

import com.github.dlomsak.regex.deriv.phase.ASTTransforms.depth5
import matryoshka.patterns.EnvT
import matryoshka._
import matryoshka.implicits._
import scalaz._
import Scalaz._
import scalaz.Cofree
import com.github.dlomsak.regex.deriv._
import RegexASTF._
import matryoshka.data._

class RETransformsSpec extends BaseSpec {
  it should "annotate a tree with depth" in {
    val r = (0, CatAST(CharAST('q'), CharAST('w')): RegexAST).ana[Cofree[RegexASTF, Int]][EnvT[Int, RegexASTF, ?]](depth5)

    println(r)



    def show(implicit F: Functor[RegexASTF]): Algebra[EnvT[Int, RegexASTF, ?], String] = {
      case EnvT((i, CharASTF(c))) => s"Character $c at depth $i"
      case EnvT((i, CatASTF(l, r))) => s"Cat at depth $i\r\n" +
        s"l: $l\r\n" +
        s"r: $r\r\n"
    }

    println("showing ", r.cata[String](show))



  }

}
