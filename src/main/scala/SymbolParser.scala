/*
ABOUT:  This includes the parser grammar for extracting symbols from mml.vct by refering to
        vocabulary_file_list(which is extracted from vocabulary directive of an article).
*/

import scala.io.Source
import org.parboiled2._
import Parser.DeliveryScheme.Throw

class SymbolParser(val input: ParserInput) extends Parser{
  val vocFileList = VocFileExtractor.vocFileUsed.flatten.map(_.toString).toList

  def listToRule(xs: List[String]) : Rule0 = {
    def loop(acc: Rule0, n: Int): Rule0 = {
      if (n==0) acc
      else loop( rule{xs(n)|acc} , n-1 )
    }
    if (xs.length>0) loop( rule{xs(0)}, xs.length-1 ) else rule{MATCH}
  }


  def inputLine = rule{ zeroOrMore(findfile ~ symbol) ~ zeroOrMore(ANY) ~ EOI }
  def findfile = rule{ zeroOrMore(!vocfile ~ ANY) ~ vocfile ~ '\n' ~
      zeroOrMore(noneOf("\n")) ~ '\n' }
  def vocfile = listToRule(vocFileList) //rule{ "SUBSET_1" | "DIRAF" }
  def symbol = rule{ oneOrMore(qualifier ~ representation ~ zeroOrMore(noneOf("\n"))).separatedBy('\n') }
  def qualifier = rule{ anyOf("ROMGUVKL") }
  def representation = rule{ capture(oneOrMore(noneOf(" " ++ "\t" ++ "\n" ++ ":"))) }

}

object SymbolExtractor {
  val filelines = Source.fromFile("mml.vct").getLines.toList
  var input = "" // could be error prone.
  for(line <- filelines) {
    input = input + line + "\n"
  }

  val symbolUsed = new SymbolParser(input).inputLine.run()

}
