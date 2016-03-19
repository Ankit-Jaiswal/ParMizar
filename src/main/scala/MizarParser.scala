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
    "->" | ".=" | "..." | "$10" | "$2"	| "$3" | "$4"	| "$5" | "$6" |"$7"	| "$8" |
    "$9" | "$1" | "(#" | "#)"
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

  def reserveSegment = rule{ reserveIden ~ "for" ~ typeExpr }
  def reserveIden = rule{ oneOrMore(identifier).separatedBy(",") }
  def definitionalBlock = rule{ defItem | definition | redefinition }


  def registrationBlock = rule{ lociDecl | clusterRegistration | identifyRegistration |
        propertyRegistration | reductionRegistration | auxiliaryItem }
  def notationBlock = rule{ lociDecl | notationDecl }



  def defItem = rule{ lociDecl | permissiveAssump | auxiliaryItem }
  def notationDecl = rule{ attributeSynonym | attributeAntonym | functorSynonym |
        modeSynonym | predicateSynonym | predicateAntonym }
  def lociDecl = rule { "let" ~ qualifiedVars ~ optional("such" ~ conditions) }
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
  def specification = rule{ "->" ~ typeExpr }
  def modeDef = rule{ "mode" ~ modePattern ~
    ( optional(specification) ~ optional("means" ~ definiens) ~ ";" ~ correctConditions |
    "is" ~ typeExpr ~ ";" ) ~ zeroOrMore(modeProperty) }
  def modePattern = rule{ modeSymbol ~ optional("of" ~ loci) }
  def modeSymbol = rule{ symbol | "set" }
  def modeSynonym = rule{ "synonym" ~ modePattern ~ "for" ~ modePattern ~ ";"}
  def definiens = rule{ simpleDefiniens | conditionalDefiniens }
  def simpleDefiniens = rule{ optional(":" ~ identifier ~ ":") ~ (sentence | termExpr) }
  def conditionalDefiniens = rule{ optional(":" ~ identifier ~ ":") ~
        oneOrMore(partialDefiniens) ~ optional("otherwise" ~ (sentence | termExpr)) }
  def partialDefiniens = rule { (sentence | termExpr) ~ "if" ~ sentence }
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

  def predDef = rule{ "pred" ~ predicatePattern ~ optional("means" ~ definiens) ~
      ";" ~ correctConditions ~ zeroOrMore(predicateProperty)  }
  def predicatePattern = rule{ optional(loci) ~ predicateSymbol ~ optional(loci) }
  def predicateProperty = rule{ ("symmetry" | "asymmetry" | "connectedness" |
      "reflexivity" | "irreflexivity") ~ justification ~ ";" }
  def predicateSynonym = rule{ "synonym" ~ predicatePattern ~ "for" ~ predicatePattern ~ ";" }
  def predicateAntonym = rule{ "antonym" ~ predicatePattern ~ "for" ~ predicatePattern ~ ";" }
  def predicateSymbol = rule{ symbol | "=" }

  def attrDef = rule{ "attr" ~ attributePattern ~ "means" ~ definiens ~ ";" ~ correctConditions }
  def attributePattern = rule{ locus ~ "is" ~ optional(attributeLoci) ~ attributeSymbol }
  def attributeSynonym = rule{ "synonym" ~ attributePattern ~ "for" ~ attributePattern ~ ";" }
  def attributeAntonym = rule{ "antonym" ~ attributePattern ~ "for" ~ attributePattern ~ ";" }
  def attributeLoci = rule{ loci | "(" ~ loci ~ ")" }
  def attributeSymbol = rule{ symbol }

  def clusterRegistration = rule{ existentialRegis | conditionalRegis | functorialRegis }
  def existentialRegis = rule{ "cluster" ~ zeroOrMore(adjective) ~ "for" ~
      typeExpr ~ ";" ~ correctConditions }
  def adjective = rule{ optional("non") ~ optional(adjArguments) ~ attributeSymbol }
  def conditionalRegis = rule{ "cluster" ~ zeroOrMore(adjective) ~ "->" ~ zeroOrMore(adjective) ~
      "for" ~ typeExpr ~ ";" ~ correctConditions }
  def functorialRegis = rule{ "cluster" ~ termExpr ~ "->" ~ zeroOrMore(adjective) ~
      optional("for" ~ typeExpr) ~ ";" ~ correctConditions }

  def identifyRegistration = rule{ "identify" ~ functorPattern ~ "with" ~ functorPattern ~
      optional("when" ~ oneOrMore(locus ~ "=" ~ locus).separatedBy(",")) ~ ";" ~ correctConditions }
  def propertyRegistration = rule{ "sethood" ~ "of" ~ typeExpr ~ justification ~ ";" }
  def reductionRegistration = rule{ "reduce" ~ termExpr ~ "to" ~ termExpr ~
      ";" ~ correctConditions }

  def correctConditions = rule{ zeroOrMore(correctcond) ~ optional("correctness" ~ justification ~ ";") }
  def correctcond = rule{ ("existence" | "uniqueness" | "coherence" | "compatibility" |
      "consistency" | "reducibility") ~ justification ~ ";" }


  def schemeBlock = rule{ "scheme" ~ identifier ~ "{" ~ schemeParameters ~ "}" ~ ":" ~
      sentence ~ optional("provided" ~ oneOrMore(proposition).separatedBy("and")) ~
      ("proof" | ";") ~ reasoning ~ "end" }
  def schemeParameters = rule{ oneOrMore(schemeSeg).separatedBy(",") }
  def schemeSeg = rule{ predicateSeg | functorSeg }
  def predicateSeg = rule{ oneOrMore(predIdentifier).separatedBy(",") ~ "[" ~
      optional(oneOrMore(typeExpr).separatedBy(",")) ~ "]" }
  def functorSeg = rule{ oneOrMore(funcIdentifier).separatedBy(",") ~ "(" ~
      optional(oneOrMore(typeExpr).separatedBy(",")) ~ ")" ~ specification }
  def predIdentifier = rule{ identifier }
  def funcIdentifier = rule{ identifier }


  def privateDefinition = rule{ constantDef | privateFunctorDef | privatePredicateDef }
  def constantDef = rule{ "set" ~ oneOrMore(equating).separatedBy(",") ~ ";" }
  def equating = rule{ variableIden ~ "=" ~ termExpr }
  def privateFunctorDef = rule{ "deffunc" ~ privateFuncPattern ~ "=" ~ termExpr ~ ";" }
  def privatePredicateDef = rule{ "defpred" ~ privatePredPattern ~ "means" ~ sentence ~ ";" }
  def privateFuncPattern = rule{ funcIdentifier ~ "(" ~
      optional(oneOrMore(typeExpr).separatedBy(",")) ~ ")" }
  def privatePredPattern = rule{ predIdentifier ~ "[" ~
      optional(oneOrMore(typeExpr).separatedBy(",")) ~ "]" }


  def reasoning: Rule0 = rule{ zeroOrMore(reasoningItem) ~
      optional(optional("then") ~ "per" ~ "cases" ~ simplejust ~ ";" ~ (caseList | supposeList)) }
  def caseList = rule{ oneOrMore(case_) }
  def case_ : Rule0 = rule{ "case" ~ (proposition | conditions) ~ ";" ~ reasoning ~ "end" ~ ";" }
  def supposeList = rule{ oneOrMore(suppose) }
  def suppose: Rule0 = rule{ "suppose" ~ (proposition | conditions) ~ ";" ~ reasoning ~ "end" ~ ";" }

  def reasoningItem = rule{ auxiliaryItem | skeletonItem }
  def skeletonItem = rule{ generalization | assumption | conclusion | exemplification }
  def generalization = rule{ "let" ~ qualifiedVars ~ optional("such" ~ conditions) ~ ";" }
  def assumption = rule{ singleAssump | collectiveAssump | existentialAssump }
  def singleAssump = rule{ "assume" ~ proposition ~ ";" }
  def collectiveAssump = rule{ "assume" ~ conditions ~ ";" }
  def existentialAssump = rule{ "given" ~ qualifiedVars ~ optional("such" ~ conditions) ~ ";" }

  def conclusion = rule{ ("thus" | "hence") ~ (compactStatement | iterativeEquality) |
      diffuseConclusion }
  def diffuseConclusion = rule{ "thus" ~ diffuseStatement | "hereby" ~ reasoning ~ "end" ~ ";" }

  def exemplification = rule{ "take" ~ oneOrMore(example).separatedBy(",") ~ ";" }
  def example = rule{ termExpr | variableIden ~ "=" ~ termExpr }

  def statement = rule{ optional("then") ~ linkableStatement | diffuseStatement }
  def linkableStatement = rule{ compactStatement | choiceStatement | typechangingStatement |
      iterativeEquality }
  def compactStatement = rule{ proposition ~ justification ~ ";" }
  def choiceStatement = rule{ "consider" ~ qualifiedVars ~ "such" ~ conditions ~
      simplejust ~ ";" }
  def typechangingStatement = rule{ "reconsider" ~ typeChangeList ~ "as" ~
      typeExpr ~ simplejust ~ ";" }
  def typeChangeList = rule{ oneOrMore((equating | variableIden)).separatedBy(",") }

  def iterativeEquality = rule{ optional(identifier ~ ":") ~ termExpr ~ "=" ~
      termExpr ~ simplejust ~ oneOrMore(".=" ~ termExpr ~ simplejust) ~ ";" }
  def diffuseStatement = rule{ optional(identifier ~ ":") ~ "now" ~ reasoning ~ "end" ~ ";" }
  def justification = rule{ simplejust | proof }
  def simplejust = rule{ straightforwardJust | schemeJust }
  def proof = rule{ "proof" ~ reasoning ~ "end" }
  def straightforwardJust = rule{ optional("by" ~ references) }
  def schemeJust = rule{ "from" ~ schReference ~ optional("(" ~ references ~ ")") }
  def references = rule{ oneOrMore(reference).separatedBy(",") }
  def reference = rule{ localReference | libraryReference }
  def schReference = rule{ localSchemeReference | librarySchemeReference }
  def localReference = rule{ identifier }
  def localSchemeReference = rule{ identifier }
  def libraryReference = rule{ filename ~ ":" ~
      oneOrMore((thmNum | "def" ~ defNum)).separatedBy(",") }
  def librarySchemeReference = rule{ filename ~ ":" ~ "sch" ~ schNum }
  def thmNum = rule{ numeral }
  def defNum = rule{ numeral }
  def schNum = rule{ numeral }

  def conditions = rule{ "that" ~ oneOrMore(proposition).separatedBy("and") }
  def proposition = rule{ optional(identifier ~ ":") ~ sentence }
  def sentence = rule{formulaExpr}



/////////////////////////////   Expressions   /////////////////////////////////
  def formulaExpr: Rule0 = rule{ "(" ~ formulaExpr ~ ")" |
      atomicFormulaExpr | quantifiedFormulaExpr |
      formulaExpr ~ "&" ~ formulaExpr | formulaExpr ~ "&" ~ "..." ~ "&" ~ formulaExpr |
      formulaExpr ~ "or" ~ formulaExpr | formulaExpr ~ "or" ~ "..." ~ "or" ~ formulaExpr |
      formulaExpr ~ "implies" ~ formulaExpr | formulaExpr ~ "iff" ~ formulaExpr |
      "not" ~ formulaExpr | "contradiction" | "thesis" }

  def atomicFormulaExpr: Rule0 = rule{ optional(termExprList) ~
      optional(("does"|"do") ~ "not") ~ predicateSymbol ~ optional(termExprList) ~
      zeroOrMore(optional(("does"|"do") ~ "not") ~ predicateSymbol ~ termExprList) |
      predIdentifier ~ "[" ~ optional(termExprList) ~ "]" |
      termExpr ~ "is" ~ oneOrMore(adjective) |
      termExpr ~ "is" ~ typeExpr }

  def quantifiedFormulaExpr: Rule0 = rule{ "for" ~ qualifiedVars ~
      optional("st" ~ formulaExpr) ~ ("holds" ~ formulaExpr | quantifiedFormulaExpr) |
      "ex" ~ qualifiedVars ~ "st" ~ formulaExpr }

  def qualifiedVars = rule{ implicitlyQualVars | explicitlyQualVars |
      explicitlyQualVars ~ "," ~ implicitlyQualVars }
  def implicitlyQualVars = rule{ oneOrMore(variableIden).separatedBy(",") }
  def explicitlyQualVars = rule{ oneOrMore(qualifiedSegment).separatedBy(",") }
  def qualifiedSegment = rule{ oneOrMore(variableIden).separatedBy(",") ~
      qualification }
  def qualification = rule{ ("being" | "be") ~ typeExpr }

  def typeExpr: Rule0 = rule{ "(" ~ radixType ~ ")" | zeroOrMore(adjective) ~ typeExpr |
      radixType }
  def structTypeExp = rule{ "(" ~ symbol ~ optional("over" ~ termExprList) ~ ")" |
      zeroOrMore(adjective) ~ symbol ~ optional("over" ~ termExprList) }
  def radixType = rule{ modeSymbol ~ optional("of" ~ termExprList) |
      symbol ~ optional("over" ~ termExprList) }

  def termExpr: Rule0 = rule{ "(" ~ termExprList ~ ")" |
      optional(arguments) ~ symbol ~ optional(arguments) |
      leftFuncBracket ~ termExprList ~ rightFuncBracket |
      funcIdentifier ~ "(" ~ optional(termExprList) ~ ")" |
      symbol ~ "(#" ~ termExprList ~ "#)" |
      "the" ~ symbol ~ "of" ~ termExpr |
      variableIden |
      "{" ~ termExpr ~ zeroOrMore(postqualification) ~ ":" ~ sentence ~ "}" |
      "the" ~ "set" ~ "of" ~ "all" ~ termExpr ~ zeroOrMore(postqualification) |
      numeral |
      termExpr ~ "qua" ~ typeExpr |
      "the" ~ symbol ~ "of" ~ termExpr |
      "the" ~ symbol |
      "the" ~ typeExpr |
      privateDefParameter |
      "it" }
  def termExprList = rule{ oneOrMore(termExpr).separatedBy(",") }

  def arguments = rule{ termExpr | "(" ~ termExprList ~ ")" }
  def adjArguments = rule{ termExprList | "(" ~ termExprList ~ ")" }

  def postqualification = rule{ "where" ~ oneOrMore(postqualSegment).separatedBy(",") }
  def postqualSegment = rule{ oneOrMore(identifier).separatedBy(",") ~
      optional(("is"|"are") ~ typeExpr) }

  def privateDefParameter = rule{ "$10" | "$1" | "$2" | "$3" | "$4" | "$5" |
      "$6" | "$7" | "$8" | "$9" }

}


object ParMizar {
  val filelines = Source.fromFile("sample.miz").getLines.toList
  var input = "" // could be error prone.
  for(line <- filelines) {
    input = input + line + "\n"
  }

  def main(args: Array[String]): Unit = {
    println(new MizarParser(input).InputLine.run())
  }
}
