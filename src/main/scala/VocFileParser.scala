/*
ABOUT: This include the parser grammar for extracting list of voc files
       form vocabulary directive of an article.
*/

import scala.language.implicitConversions
import org.parboiled2._
import Parser.DeliveryScheme.Throw

class VocFileParser(val input: ParserInput) extends Parser {
  def ws = rule{ "\t" | " " }
  def nl = rule{ "\n" }
  def comment = rule{ "::" ~ zeroOrMore(noneOf("\n")) }
  implicit def wspStr(s: String) = rule{ str(s) ~ zeroOrMore(ws | comment | nl) }

  def vocDirective = rule{ zeroOrMore(oneOrMore(!"vocabularies" ~ ANY) ~ "vocabularies" ~
      oneOrMore(filename).separatedBy(",") ~ ";") ~ zeroOrMore(ANY) ~ EOI }
  def filename = rule{ capture(oneOrMore(CharPredicate.AlphaNum | '_')) }
}

object VocFileExtractor {
  val input = scala.io.Source.fromFile("sample.miz").getLines mkString "\n"
  val vocFileUsed = new VocFileParser(input).vocDirective.run()

}
