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

object ParMizar {
  val input = scala.io.Source.fromFile(filename).getLines mkString "\n"
  def from(filename: String) = new ArticleParser(input).InputLine.run()
}
