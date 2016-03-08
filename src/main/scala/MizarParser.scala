import scala.io.Source
import scala.language.implicitConversions
import org.parboiled2._
import MizarLang._


class MizarParser(val input: ParserInput) extends Parser {

  def ws = rule{ "\t" | " " }
  def nl = rule{ "\n" }
  def comment = rule{ "::" ~ zeroOrMore(noneOf("\n")) }
  implicit def wspStr(s: String) = rule{ str(s) ~ zeroOrMore(ws | comment | nl) }



///////////////////////////////   tokens   ///////////////////////////////////
  def numeral = rule{ oneOrMore(CharPredicate.Digit) }
  def filename = rule{ oneOrMore(CharPredicate.AlphaNum | "_") }
  def hiddenSymbol = rule{
    "," | ";"	| ":"	| "(" | ")" | "["	| "]"	| "{" | "}"	| "=" | "<>" | "&" |
    "->" | ".=" | "..." | "$1" | "$2"	| "$3" | "$4"	| "$5" | "$6" |"$7"	| "$8" |
    "$9" | "$10" | "(#" | "#)"
  }
  def identifier = rule{ !hiddenSymbol ~ !numeral ~
    oneOrMore(CharPredicate.AlphaNum | "_" | "'") }
  def symbol = rule{ !numeral ~ noneOf(" " ++ "\t" ++ "\n") }



////////////////////////////    article   ////////////////////////////////////
  def InputLine = rule{ zeroOrMore(comment | nl | ws) ~ article ~ EOI }

  def article = rule{ environDecl ~ textproper }

  def environDecl = rule{ "environ" ~ zeroOrMore(directive) }
  def directive = rule{ vocDirective | libDirective | reqDirective }
  def vocDirective = rule{ "vocabularies" ~ oneOrMore(filename).separatedBy(", ") ~ ";" }
  def libDirective = rule{
    ( "notations" | "constructors" | "registrations" | "definitions" | "expansions"
    | "equalities" | "theorems" | "schemes" ) ~ oneOrMore(filename).separatedBy(", ") ~ ";"
  }
  def reqDirective = rule{ "requirements" ~ oneOrMore(filename).separatedBy(", ") ~ ";" }



////////////////////////////  textproper  /////////////////////////////////////
  def textproper = rule{ oneOrMore(section) }
  def section = rule{ "begin" ~ zeroOrMore(textItem) }
  def textItem = rule{ reservation | definitionalItem | registrationItem |
        notationItem | theorem | schemeItem | auxiliaryItem }

  def reservation = rule{ "reserve" ~ oneOrMore(reserveSegment).separatedBy(",") ~ ";" }
  def definitionalItem = rule{ "definition" ~ zeroOrMore(definitionalBlock) ~ "end" ~ ";" }
  def registrationItem = rule{ "registration" ~ zeroOrMore(registrationBlock) ~ "end" ~ ";" }
  def notationItem = rule{ "notation" ~ zeroOrMore(notationBlock) ~ "end" ~ ";" }
  def theorem = rule{ "theorem" ~ compactStatement }
  def schemeItem = rule{ schemeBlock ~ ";" }
  def auxiliaryItem = rule{ statement | privateDefinition }

  def reserveSegment = rule{ reserveIden ~ "for" ~ typeExpression }
  def reserveIden = rule{ oneOrMore(identifier).separatedBy(",") }
  def definitionalBlock = rule{ defItem | definition | redefinition }


  def registrationBlock = rule{ lociDecl | clusterRegistration | identifyRegistration |
        propertyRegistration | reductionRegistration | auxiliaryItem }
  def notationBlock = rule{ lociDecl | notationDecl }
  def compactStatement = rule{ MISMATCH0 }
  def schemeBlock = rule{ MISMATCH0 }
  def statement = rule{ MISMATCH0 }
  def privateDefinition = rule{ MISMATCH0 }



  def defItem = rule{ lociDecl | permissiveAssump | auxiliaryItem }
  def notationDecl = rule{ attributeSynonym | attributeAntonym | functorSynonym |
        modeSynonym | predicateSynonym | predicateAntonym }
  def lociDecl = rule { "let" ~ qualifiedVar ~ optional("such" ~ conditions) }
  def permissiveAssump = rule{ assumption }
  def definition = rule{ structDef | modeDef | funcDef | predDef | attrDef }
  def redefinition = rule{ "redefine" ~ ( modeDef | funcDef | predDef | attrDef ) }
  def structDef = rule{ "struct" ~ optional("(" ~ ancestors ~ ")") ~ symbol ~
        optional("over" ~ loci) ~ "(#" ~ fields ~ "#)" ~ ";" }
  def ancestors = rule{ oneOrMore(structTypeExp).separatedBy(",") }
  def loci = rule{ oneOrMore(locus).separatedBy(",") }
  def fields = rule{ oneOrMore(fieldSegment).separatedBy(",") }
  def locus = rule{ variableIden }
  def variableIden = rule{ identifier }
  def fieldSegment = rule{ oneOrMore(symbol).separatedBy(",") ~ specification }
  def specification = rule{ "->" ~ typeExpression }
  def modeDef = rule{ "mode" ~ modePattern ~
    ( optional(specification) ~ optional("means" ~ definiens) ~ ";" ~ correctConditions |
    "is" ~ typeExpression ~ ";" ) ~ zeroOrMore(modeProperty) }
  def modePattern = rule{ modeSymbol ~ optional("of" ~ loci) }
  def modeSymbol = rule{ symbol | "set" }
  def modeSynonym = rule{ "synonym" ~ modePattern ~ "for" ~ modePattern ~ ";"}
  def definiens = rule{ simpleDefiniens | conditionalDefiniens }
  def simpleDefiniens = rule{ optional(":" ~ identifier ~ ":") ~ (sentence | termExpression) }
  def conditionalDefiniens = rule{ optional(":" ~ identifier ~ ":") ~
        oneOrMore(partialDefiniens) ~ optional("otherwise" ~ (sentence | termExpression)) }
  def partialDefiniens = rule { (sentence | termExpression) ~ "if" ~ sentence }
  def modeProperty = rule{ "sethood" ~ justification ~ ";" }


  def funcDef = rule{ "func" ~ functorPattern ~ optional(specification) ~
      optional(("means" | "equals") ~ definiens) ~ ";" ~ correctConditions ~
      zeroOrMore(functorProperty) }
  def functorPattern = rule{ optional(functorloci) ~ symbol ~ optional(functorloci) |
      leftFuncBracket ~ loci ~ rightFuncBracket }
  def functorProperty = rule{ ( "commutativity" | "idempotence" | "involutiveness" |
      "projectivity" ) ~ justification ~ ";" }
  def functorSynonym = rule{ "synonym" ~ functorPattern ~ "for" ~ functorPattern ~ ";" }
  def functorloci = rule{ locus | "(" ~ loci ~ ")" }
  def leftFuncBracket = rule{ symbol | "{" | "[" }
  def rightFuncBracket = rule{ symbol | "}" | "]" }


  def clusterRegistration = rule{MISMATCH0}
  def identifyRegistration = rule{MISMATCH0}
  def propertyRegistration = rule{MISMATCH0}
  def reductionRegistration = rule{MISMATCH0}
  def attributeSynonym = rule{MISMATCH0}
  def attributeAntonym = rule{MISMATCH0}
  def predicateSynonym = rule{MISMATCH0}
  def predicateAntonym = rule{MISMATCH0}
  def qualifiedVar = rule{ MISMATCH0 }
  def conditions = rule{MISMATCH0}
  def assumption = rule{MISMATCH0}

  def predDef = rule{MISMATCH0}
  def attrDef = rule{MISMATCH0}
  def correctConditions = rule{MISMATCH0}
  def justification = rule{MISMATCH0}

  def sentence = rule{MISMATCH0}
  def termExpression = rule{MISMATCH0}
  def structTypeExp = rule{MISMATCH0}
  def typeExpression = rule{ MISMATCH0 }
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
