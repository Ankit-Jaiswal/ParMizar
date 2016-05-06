/*
ABOUT: This is the main file of the project, which will parses sample.miz
        in three steps:
        1. Extraction of voc files wrriten in vocabulary directive of an article.
        2. Extraction of symbols from each voc file.
        3. Parsing the article.
*/
import MizarLang._
import org.parboiled2._
import Parser.DeliveryScheme.Throw

case class ParMizar(filename: String) {
  val input = scala.io.Source.fromFile(filename).getLines mkString "\n"
  val tree = new ArticleParser(input).InputLine.run()
}

/*
object ParMizar {
  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("sample.miz").getLines mkString "\n"
    println(new ArticleParser(input).InputLine.run())
  }
}
*/
