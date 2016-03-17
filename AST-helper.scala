import scala.util.parsing.combinator._

package ASTHelper {

  case class MetaAST(nts: List[Nonterminal]) {
    override def toString =
      nts.map(s => s.toString + "\n").foldLeft("")(_+_)
  }
  sealed abstract class Nonterminal
  case class RegularNonterminal(
    name: String,
    prods: List[Production]) extends Nonterminal {
    override def toString = {
      val header = s"sealed abstract class $name\n"
      val prodstrs =
        prods.map(s => s.toString + s" extends $name\n").foldLeft("")(_+_)
      header + prodstrs
    }
  }
  case class SingletonNonterminal(
    p: NonZeroArityProduction
  ) extends Nonterminal {
    override def toString = p.toString
  }
  sealed abstract class Production
  case class NonZeroArityProduction(
    prodname: String,
    components: List[Component]
  ) extends Production {
    override def toString = {
      val header = s"case class $prodname("
      val middle =
        if (components.nonEmpty)
          components.init.map(_.toString ++ ", ").foldLeft("")(_+_)
        else ""
      val last =
        if (components.nonEmpty)
          components.last.toString
        else ""
      val end = ")"
      header + middle + last + end
    }
  }
  case class ZeroArityProduction(prodname: String) extends Production {
    override def toString = s"case object $prodname"
  }
  case class Component(varname: String, ty: String) {
    override def toString = {
      s"$varname: $ty"
    }
  }

  object ASTHelper extends RegexParsers with PackratParsers {

    lazy val capitalIdent: Parser[String] = """[A-Z][A-Za-z]*""".r
    lazy val ident: Parser[String] = """[_A-Za-z][_A-Za-z0-9]*""".r

    lazy val ast: Parser[MetaAST] =
      rep(nonterminal) ^^ { MetaAST(_) }
    lazy val nonterminal: Parser[Nonterminal] =
      nonterminal1 | nonterminal2
    lazy val nonterminal1: Parser[Nonterminal] =
      capitalIdent ~ "::=" ~ repsep(production, "|") ^^
        { case name ~ "::=" ~ prods => RegularNonterminal(name, prods) }
    lazy val nonterminal2: Parser[Nonterminal] =
      nonzeroarityproduction ^^ { SingletonNonterminal(_) }
    lazy val production: Parser[Production] =
      nonzeroarityproduction | zeroarityproduction
    lazy val nonzeroarityproduction: Parser[NonZeroArityProduction] =
      capitalIdent ~ "(" ~ repsep(component, ",") ~ ")" ^^
        { case id ~ "(" ~ cs ~ ")" =>
          NonZeroArityProduction(id, fixRepeats(cs)) }
    lazy val zeroarityproduction: Parser[ZeroArityProduction] =
      capitalIdent ^^ { ZeroArityProduction(_) }
    lazy val component: Parser[Component] =
      withVarName | withoutVarName
    lazy val withVarName: Parser[Component] =
      ident ~ ":" ~ capitalIdent ^^ { case id ~ ":" ~ nt => Component(id, nt) }
    lazy val withoutVarName: Parser[Component] =
      capitalIdent ^^
        { case id => Component(id.toLowerCase.substring(0,1), id) }

    def fixRepeats(cs: List[Component]): List[Component] = {
      val names = cs.map(_.varname)
      val duplicates = names.diff(names.distinct).toSet.toList
      var currentIndex = Map[String, Int]() ++ duplicates.map(_ -> 1)
      cs.foldLeft(List[Component]())({case (acc, Component(n, ty)) => {
        if (currentIndex.contains(n)) {
          val newn = n + currentIndex(n)
          currentIndex = currentIndex + (n -> (currentIndex(n) + 1))
          acc :+ Component(newn, ty)
        } else {
          acc :+ Component(n, ty)
        }
      }})
    }

    def removeComments(input: String): String = {
      """//[^\n]*""".r.replaceAllIn(input, "")
    }

    def main(args: Array[String]) {
      val input = args(0)
      val file = removeComments(scala.io.Source.fromFile(input).mkString)
      val (shouldSaveFile, outfile) = args.toList match {
        case _::"-o"::f::_ => (true, f)
        case _ => (false, "")
      }

      val res: String = parseAll(ast, file) match {
        case Success(metaast, _) => println(metaast); metaast.toString
        case x => throw new Exception(x.toString)
      }

      if (shouldSaveFile) {
        import java.io._
        new PrintWriter(outfile) { write(res.toString); close }
      }
    }
  }

}
