/*
ABOUT:  This includes the parser grammar for extracting symbols from mml.vct by refering to
        vocabulary_file_list(which is extracted from vocabulary directive of an article).
*/

import org.parboiled2._
import Parser.DeliveryScheme.Throw

class SymbolParser(val input: ParserInput) extends Parser{
  def getVoc(filename: String) = rule{ find("#"++filename++"\n") ~
      zeroOrMore(noneOf("\n")) ~ '\n' ~ symbolDef }
  def find(s: String) = rule{ zeroOrMore(!s ~ ANY) ~ s }

  def symbolDef = rule{ oneOrMore(getSymbol ~ zeroOrMore(noneOf("\n"))).separatedBy('\n') }
  def getSymbol = rule{ capture(qualifier ~ representation) }
  def qualifier = rule{ anyOf("ROMGUVKL") }
  def representation = rule{ oneOrMore(noneOf(" " ++ "\t" ++ "\n")) }

}

object SymbolExtractor {
  val input = scala.io.Source.fromFile("mml.vct").getLines mkString "\n"
  val vocFileList = VocFileExtractor.vocFileUsed.flatten.map(_.toString).toList
  val symbolUsed = vocFileList.map(filename => new SymbolParser(input).getVoc(filename).run())

}
