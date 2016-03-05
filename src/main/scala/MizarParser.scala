import scala.io.Source
import scala.language.implicitConversions
import org.parboiled2._
import MizarLang._


class MizarParser(val input: ParserInput) extends Parser {

  def ws = rule { "\t" | " " }
  def nl = rule { "\n" }
  def comment = rule{ "::" ~ zeroOrMore(noneOf("\n")) }
  implicit def wspStr(s: String) = rule { str(s) ~ zeroOrMore(ws | comment | nl) }



///////////////////////////////   tokens   ///////////////////////////////////
  def numeral:Rule1[Numeral] = rule{ capture(oneOrMore(CharPredicate.Digit)) ~>
        ((s: String) => Numeral(s)) }
  def filename: Rule1[FileName] = rule { capture(oneOrMore(CharPredicate.AlphaNum | "_")) ~>
        ((s: String) => FileName(s)) }
  def hiddenSymbol = rule {
    "," | ";"	| ":"	| "(" | ")" | "["	| "]"	| "{" | "}"	| "=" | "<>" | "&" |
    "->" | ".=" | "..." | "$1" | "$2"	| "$3" | "$4"	| "$5" | "$6" |"$7"	| "$8" |
    "$9" | "$10" | "(#" | "#)"
  }
  def identifier: Rule1[Identifier] = rule { !hiddenSymbol ~ !numeral ~
        capture(oneOrMore(CharPredicate.AlphaNum | "_" | "'") ~>
        ((s: String) => Identifier(s)) }
  def symbol: Rule1[Symbol] = rule { !numeral ~ capture(noneOf(" " ++ "\t" ++ "\n")) ~>
        ((s: String) => Symbol(s))}



////////////////////////////    article   ////////////////////////////////////
  def InputLine = rule{ zeroOrMore(comment | nl | ws) ~ article ~ EOI }

  def article = rule{ environDecl ~ textproper }

  def environDecl = rule { "environ" ~ zeroOrMore(directive) }
  def directive = rule { vocDirective | libDirective | reqDirective }
  def vocDirective = rule { "vocabularies" ~ oneOrMore(filename).separatedBy(", ") ~ ";" }
  def libDirective = rule {
    ( "notations" | "constructors" | "registrations" | "definitions" | "expansions"
    | "equalities" | "theorems" | "schemes" ) ~ oneOrMore(filename).separatedBy(", ") ~ ";"
  }
  def reqDirective = rule{ "requirements" ~ oneOrMore(filename).separatedBy(", ") ~ ";" }



////////////////////////////  textproper  /////////////////////////////////////
  def textproper = rule{ "begin" }




}


object ParMizar {
  val filelines = Source.fromFile("sample.miz").getLines.toList
  var input = "" // could be error proone.
  for(line <- filelines) {
    input = input + line + "\n"
  }

  def main(args: Array[String]): Unit = {
    println(new MizarParser(input).InputLine.run())
  }
}
