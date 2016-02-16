import scala.io.Source
import org.parboiled2._
import MizarLang._


class MizarParser(val input: ParserInput) extends Parser {
  def ws = rule { oneOrMore(" " | "\t") }
  def nl = rule { oneOrMore("\n") }

  def article = rule{ environDecl ~ textproper }
  def environDecl = rule{ "environ" ~ nl }
  def textproper = rule{ "begin" ~ nl }
}


object ParMizar {
  val filelines = Source.fromFile("sample.miz").getLines.toList
  var input = ""
  for(line <- filelines) {
    input = input + line + "\n"
  }

  def main(args: Array[String]): Unit = {
    println(new MizarParser(input).article.run())
  }
}
