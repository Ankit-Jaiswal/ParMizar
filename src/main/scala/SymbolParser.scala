/*
ABOUT:  This includes the parser grammar for extracting symbols from mml.vct by refering to
        vocabulary_file_list(which is extracted from vocabulary directive of an article).
*/

import scala.io.Source
import org.parboiled2._
import Parser.DeliveryScheme.Throw

class SymbolParser(val input: ParserInput) extends Parser{
  def getVoc(filename: String) = rule{ find("#"++filename++"\n") ~
      zeroOrMore(noneOf("\n")) ~ '\n' ~ getSymbols }
  def find(s: String) = rule{ zeroOrMore(!s ~ ANY) ~ s }

  def getSymbols = rule{ oneOrMore(qualifier ~ representation ~
      zeroOrMore(noneOf("\n"))).separatedBy('\n') }
  def qualifier = rule{ anyOf("ROMGUVKL") }
  def representation = rule{ capture(oneOrMore(noneOf(" " ++ "\t" ++ "\n"))) }

}

object SymbolExtractor {
  val filelines = Source.fromFile("mml.vct").getLines.toList
  var input = "" // could be error prone.
  for(line <- filelines) {
    input = input + line + "\n"
  }

  val vocFileList = VocFileExtractor.vocFileUsed.flatten.map(_.toString).toList
  val symbolUsed = vocFileList.map(filename => new SymbolParser(input).getVoc(filename).run())

}
