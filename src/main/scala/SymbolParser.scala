/*
ABOUT:  This includes the parser grammar for extracting symbols from the
        vocabulary_files(which is extracted from vocabulary directive of an article).

STATUS: Currently, it is only able to extract symbols from hidden.voc
        VocFileParser  is still remaining to be integrated within this. 
*/

import scala.io.Source
import org.parboiled2._
import Parser.DeliveryScheme.Throw

class SymbolParser(val input: ParserInput) extends Parser{

  def inputLine = rule{ mizfile ~ getSymbol ~ zeroOrMore(ANY) ~ EOI }
  def mizfile = rule{ zeroOrMore(!"#HIDDEN" ~ ANY) ~ "#HIDDEN" ~ '\n' ~
      oneOrMore(noneOf("\n")) ~ '\n' }
  def getSymbol = rule{ oneOrMore(qualifier ~ representation).separatedBy('\n') }
  def qualifier = rule{ anyOf("ROMGUVKL") }
  def representation = rule{ capture(oneOrMore(noneOf(" " ++ "\t" ++ "\n" ++ ":"))) }

}

object SymbolExtractor {
  val vocFilelines = Source.fromFile("mml.vct").getLines.toList
  var vocInput = "" // could be error prone.
  for(line <- vocFilelines) {
    vocInput = vocInput + line + "\n"
  }

  val symbolUsed = new SymbolParser(vocInput).inputLine.run()
}
