/*
ABOUT: This is the main file of the project, which will parses sample.miz
        in three steps:
        1. Extraction of voc files wrriten in vocabulary directive of an article.
        2. Extraction of symbols from each voc file.
        3. Parsing the article.
*/


import scala.io.Source

object runParMizar {
  val filelines = Source.fromFile("sample.miz").getLines.toList
  var input = "" // could be error prone.
  for(line <- filelines) {
    input = input + line + "\n"
  }

  def main(args: Array[String]): Unit = {
    println(new ArticleParser(input).InputLine.run())
  }
}
