/*
ABOUT : This includes the parser grammar for parsing a mizar article.

STATUS: Left recursion removed. Able to parse smallest article in MML i.e. hidden.miz
To do:  Extracting notations and constructors from "imported" files.
*/

import scala.language.implicitConversions
import org.parboiled2._
import MizarLang._


class ArticleParser(val input: ParserInput) extends Parser {

  def ws = rule{ "\t" | " " }
  def nl = rule{ "\n" }
  def comment = rule{ "::" ~ zeroOrMore(noneOf("\n")) }
  implicit def wspStr(s: String) = rule{ str(s) ~ zeroOrMore(ws | comment | nl) }

  // rw := reserved_words  and   sym := symbol
  val rwList = scala.io.Source.fromFile("reserved_words.txt").getLines.toList.reverse
  val hidden = List("Mobject", "R<>", "Rin", "Vstrict")
  val symList = SymbolExtractor.symbolUsed.flatten.map(_.toString).toList ++ hidden

  val structSymList = symList.filter(_.head == 'G').map(_.drop(1))
  val selectorSymList = symList.filter(_.head == 'U').map(_.drop(1))
  val funcSymList = symList.filter(_.head == 'O').map(_.drop(1))
  val modeSymList = symList.filter(_.head == 'M').map(_.drop(1))
  val predicateSymList = symList.filter(_.head == 'R').map(_.drop(1))
  val attributeSymList = symList.filter(_.head == 'V').map(_.drop(1))
  val leftFuncBracketList = symList.filter(_.head == 'K').map(_.drop(1))
  val rightFuncBracketList = symList.filter(_.head == 'L').map(_.drop(1))

  def listToRule(xs: List[String]) : Rule0 = {
    def loop(acc: Rule0, n: Int): Rule0 = {
      if (n==0) acc
      else loop( rule{xs(n-1)|acc} , n-1 )
    }
    if (xs.length>0) loop( rule{xs.last}, xs.length-1 ) else rule{MISMATCH}
  }

  def rw = rule{ listToRule(rwList) ~ endM }
  def spSym = rule{ ":" | ";" | "," | "(#" | "#)" | "(" | ")" |
      "[" | "]" | "{" | "}" | "=" | ".=" | "&" | "->" }
  def endM = rule{ oneOrMore(ws | nl) | &(spSym) }


///////////////////////////////   tokens   ///////////////////////////////////
  def symbol = rule{ listToRule(symList.map(_.drop(1)).sorted.reverse) }
  def numeral: Rule1[Numeral] = rule{ (!'0' ~ capture(oneOrMore(CharPredicate.Digit)) ~ zeroOrMore(ws|nl)) ~> ((s: String) => Numeral(s)) }
  def filename: Rule1[FileName] = rule{ ( &(CharPredicate.Alpha) ~ capture(oneOrMore(CharPredicate.AlphaNum | '_')) ~
      zeroOrMore(ws|nl) ) ~> ((s: String) => FileName(s)) }
  def identifier: Rule1[Identifier] = rule{ (capture(oneOrMore(CharPredicate.AlphaNum | '_' | "'")) ~ zeroOrMore(ws|nl)) ~> ((s: String) => Identifier(s)) }


  def structSymbol: Rule1[Symbol] = rule{ capture(symbol) ~ test(structSymList.contains(valueStack.peek)) ~> ((s: String) => Symbol(s)) }
  def selectorSymbol: Rule1[Symbol] = rule{ capture(symbol) ~ test(selectorSymList.contains(valueStack.peek)) ~> ((s: String) => Symbol(s)) }
  def funcSymbol: Rule1[Symbol] = rule{ capture(symbol) ~ test(funcSymList.contains(valueStack.peek)) ~> ((s: String) => Symbol(s)) }
  def modeSymbol: Rule1[Symbol] = rule{ capture(symbol) ~ test(modeSymList.contains(valueStack.peek)) ~> ((s: String) => Symbol(s)) | "set" ~ push(Symbol("set")) }
  def predicateSymbol: Rule1[Symbol] = rule{ capture(symbol) ~ test(predicateSymList.contains(valueStack.peek)) ~> ((s: String) => Symbol(s)) | "=" ~ push(Symbol("=")) }
  def attributeSymbol: Rule1[Symbol] = rule{ capture(symbol) ~ test(attributeSymList.contains(valueStack.peek)) ~> ((s: String) => Symbol(s)) }
  def leftFuncBracket: Rule1[Symbol] = rule{ capture(symbol) ~ test(leftFuncBracketList.contains(valueStack.peek)) ~> ((s: String) => Symbol(s)) |
      "{" ~ push(Symbol("{")) | "[" ~ push(Symbol("[")) }
  def rightFuncBracket: Rule1[Symbol] = rule{ capture(symbol) ~ test(rightFuncBracketList.contains(valueStack.peek)) ~> ((s: String) => Symbol(s)) |
      "}" ~ push(Symbol("}")) | "]" ~ push(Symbol("]")) }



////////////////////////////    article   ////////////////////////////////////
  def InputLine = rule{ zeroOrMore(comment | nl | ws) ~ article ~ EOI }

  def article: Rule1[Article] = rule{ (environDecl ~ textproper) ~> ((e: EnvironmentDeclaration, t: TextProper) => Article(e,t)) }

  def environDecl: Rule1[EnvironmentDeclaration] = rule{ ("environ" ~ zeroOrMore(directive)) ~> ((xs: Seq[Directive]) => EnvironmentDeclaration(xs.toList)) }
  def directive: Rule1[Directive] = rule{ vocDirective | libDirective | reqDirective }
  def vocDirective: Rule1[VocabularyDirective] = rule{ ("vocabularies" ~ oneOrMore(filename).separatedBy(",") ~ ";") ~>
      ((xs: Seq[FileName]) => VocabularyDirective(xs.toList)) }
  def libDirective: Rule1[LibraryDirective] = rule{
    ("notations" ~ oneOrMore(filename).separatedBy(",") ~ ";") ~> ((xs: Seq[FileName]) => Notations(xs.toList)) |
    ("constructors" ~ oneOrMore(filename).separatedBy(",") ~ ";") ~> ((xs: Seq[FileName]) => Constructors(xs.toList)) |
    ("registrations" ~ oneOrMore(filename).separatedBy(",") ~ ";") ~> ((xs: Seq[FileName]) => Registrations(xs.toList)) |
    ("definitions" ~ oneOrMore(filename).separatedBy(",") ~ ";") ~> ((xs: Seq[FileName]) => Definitions(xs.toList)) |
    ("expansions" ~ oneOrMore(filename).separatedBy(",") ~ ";") ~> ((xs: Seq[FileName]) => Expansions(xs.toList)) |
    ("equalities" ~ oneOrMore(filename).separatedBy(",") ~ ";") ~> ((xs: Seq[FileName]) => Equalities(xs.toList)) |
    ("theorems" ~ oneOrMore(filename).separatedBy(",") ~ ";") ~> ((xs: Seq[FileName]) => Theorems(xs.toList)) |
    ("schemes" ~ oneOrMore(filename).separatedBy(",") ~ ";") ~> ((xs: Seq[FileName]) => Schemes(xs.toList))
  }
  def reqDirective: Rule1[RequirementDirective] = rule{ ("requirements" ~ oneOrMore(filename).separatedBy(", ") ~ ";") ~>
      ((xs: Seq[FileName]) => RequirementDirective(xs.toList)) }



////////////////////////////  textproper  /////////////////////////////////////
  def textproper: Rule1[TextProper] = rule{ oneOrMore(section) ~> ((xs: Seq[Section]) => TextProper(xs.toList)) }
  def section: Rule1[Section] = rule{ ("begin" ~ zeroOrMore(textItem)) ~> ((xs: Seq[TextItem]) => Section(xs.toList)) }
  def textItem: Rule1[TextItem] = rule{ reservation | definitionalItem | registrationItem |
        notationItem | theorem | schemeItem | auxiliaryItem }
  def reservation: Rule1[Reservation] = rule{ ("reserve" ~ oneOrMore(reserveSegment).separatedBy(",") ~ ";") ~> ((xs: Seq[ReservationSegment]) => Reservation(xs.toList)) }
  def definitionalItem: Rule1[DefinitionalItem] = rule{ ("definition" ~ zeroOrMore(definitionalBlock) ~ "end" ~ ";") ~> ((xs: Seq[DefinitionalBlock]) => DefinitionalItem(xs.toList)) }
  def registrationItem: Rule1[RegistrationItem] = rule{ ("registration" ~ zeroOrMore(registrationBlock) ~ "end" ~ ";") ~> ((xs: Seq[RegistrationBlock]) => RegistrationItem(xs.toList)) }
  def notationItem: Rule1[NotationItem] = rule{ ("notation" ~ zeroOrMore(notationBlock) ~ "end" ~ ";") ~> ((xs: Seq[NotationBlock]) => NotationItem(xs.toList)) }
  def theorem: Rule1[Theorem] = rule{ ("theorem" ~ compactStatement) ~> ((c: CompactStatement) => Theorem(c)) }
  def schemeItem: Rule1[SchemeItem] = rule{ (schemeBlock ~ ";") ~> ((s: SchemeBlock) => SchemeItem(s)) }
  def auxiliaryItem: Rule1[AuxiliaryItem] = rule{ statement | privateDefinition }

  def reserveSegment: Rule1[ReservationSegment] = rule{ (reserveIden ~ "for" ~ typeExpr) ~> ((r: ReservedIdentifiers, t: TypeExpression) => ReservationSegment(r,t)) }
  def reserveIden: Rule1[ReservedIdentifiers] = rule{ oneOrMore(identifier).separatedBy(",") ~> ((xs: Seq[Identifier]) => ReservedIdentifiers(xs.toList)) }
  def definitionalBlock: Rule1[DefinitionalBlock] = rule{ defItem | definition | redefinition }
  def registrationBlock: Rule1[RegistrationBlock] = rule{ lociDecl | clusterRegistration | identifyRegistration |
        propertyRegistration | reductionRegistration | auxiliaryItem }
  def notationBlock: Rule1[NotationBlock] = rule{ lociDecl | notationDecl }

  def defItem: Rule1[DefinitionItem] = rule{ lociDecl | permissiveAssump | auxiliaryItem }
  def notationDecl: Rule1[NotationDeclaration] = rule{ attributeSynonym | attributeAntonym | functorSynonym |
        modeSynonym | predicateSynonym | predicateAntonym }
  def lociDecl: Rule1[LociDeclaration] = rule { ("let" ~ qualifiedVars ~ optional("such" ~ conditions) ~ ";") ~>
      ((q: QualifiedVariables, c: Option[Conditions]) => LociDeclaration(q,c)) }
  def permissiveAssump: Rule1[PermissiveAssumption] = rule{ assumption ~> ((a: Assumption) => PermissiveAssumption(a)) }

  def definition: Rule1[Definition] = rule{ structDef | modeDef | funcDef | predDef | attrDef }
  def redefinition: Rule1[Redefinition] = rule{ "redefine" ~ ( modeDef | funcDef | predDef | attrDef ) }

  def structDef: Rule1[StructureDefinition] = rule{ ("struct" ~ optional("(" ~ ancestors ~ ")") ~ structSymbol ~
        optional("over" ~ loci) ~ "(#" ~ fields ~ "#)" ~ ";") ~> ((optA: Option[Ancestors], s: Symbol, optL: Option[Loci], xs: List[FieldSegment]) => StructureDefinition(optA,s,optL,xs)) }
  def ancestors: Rule1[Ancestors] = rule{ oneOrMore(structTypeExp).separatedBy(",") ~> ((xs: Seq[StructureTypeExpression]) => Ancestors(xs.toList)) }
  def loci: Rule1[Loci] = rule{ oneOrMore(locus).separatedBy(",") ~> ((xs: Seq[Locus]) => Loci(xs.toList)) }
  def fields: Rule1[List[FieldSegment]] = rule{ oneOrMore(fieldSegment).separatedBy(",") ~> ((xs: Seq[FieldSegment]) => xs.toList) }
  def locus: Rule1[Locus] = rule{ variableIden ~> ((v: VariableIdentifier) => Locus(v)) }
  def variableIden: Rule1[VariableIdentifier] = rule{ identifier ~> ((i: Identifier) => VariableIdentifier(i)) }
  def fieldSegment: Rule1[FieldSegment] = rule{ (oneOrMore(selectorSymbol).separatedBy(",") ~ specification) ~> ((xs: Seq[Symbol], sp: Specification) => FieldSegment(xs.toList,sp)) }
  def specification: Rule1[Specification] = rule{ "->" ~ typeExpr ~> ((t: TypeExpression) => Specification(t)) }

  def modeDef: Rule1[ModeDefinition] = rule{ ( "mode" ~ modePattern ~
      ( (optional(specification) ~ optional("means" ~ definiens) ~ ";" ~ correctConditions) ~>
      ((optS: Option[Specification], optD: Option[Definiens], cc: CorrectConditions) => ModeCondBlock(optS,optD,cc)) |
      ("is" ~ typeExpr ~ ";") ~> ((t: TypeExpression) => ModeTypExpBlock(t)) ) ~ zeroOrMore(modeProperty) ) ~>
      ((m: ModePattern, b: ModeBlock, xs: Seq[ModeProperty]) => ModeDefinition(m,b,xs.toList)) }
  def modePattern: Rule1[ModePattern] = rule{ (modeSymbol ~ optional("of" ~ loci)) ~> ((s: Symbol, optL: Option[Loci]) => ModePattern(s,optL)) }
  def modeSynonym: Rule1[ModeSynonym] = rule{ ("synonym" ~ modePattern ~ "for" ~ modePattern ~ ";") ~> ((m1: ModePattern, m2: ModePattern) => ModeSynonym(m1,m2))}
  def definiens: Rule1[Definiens] = rule{ simpleDefiniens | conditionalDefiniens }
  def simpleDefiniens: Rule1[SimpleDefiniens] = rule{ (optional(":" ~ identifier ~ ":") ~ (sentence | defienTerm)) ~>
      ((optI: Option[Identifier], sORt: SentOrTerm) => SimpleDefiniens(optI,sORt)) }
  def defienTerm: Rule1[DefienTerm] = rule{ termExpr ~> ((t: TermExpression) => DefienTerm(t)) }
  def conditionalDefiniens: Rule1[ConditionalDefiniens] = rule{ ( optional(":" ~ identifier ~ ":") ~
        oneOrMore(partialDefiniens) ~ optional("otherwise" ~ (sentence | defienTerm)) ) ~>
        ((optI: Option[Identifier], xs: Seq[PartialDefiniens], opt2: Option[SentOrTerm]) => ConditionalDefiniens(optI,xs.toList,opt2)) }
  def partialDefiniens: Rule1[PartialDefiniens] = rule { ((sentence | defienTerm) ~ "if" ~ sentence) ~> ((sORt: SentOrTerm, s: Sentence) => PartialDefiniens(sORt,s)) }
  def modeProperty: Rule1[ModeProperty] = rule{ ("sethood" ~ justification ~ ";") ~> ((j: Justification) => ModeProperty(j)) }

  def funcDef: Rule1[FunctorDefinition] = rule{ ( "func" ~ functorPattern ~ optional(specification) ~
      optional(("means" | "equals") ~ definiens) ~ ";" ~ correctConditions ~
      zeroOrMore(functorProperty) ) ~>
      ((patt: FunctorPattern, optS: Option[Specification], optD: Option[Definiens], cc: CorrectConditions, xs: Seq[FunctorProperty]) => FunctorDefinition(patt,optS,optD,cc,xs.toList)) }
  def functorPattern: Rule1[FunctorPattern] = rule{ (optional(functorloci) ~ funcSymbol ~ optional(functorloci)) ~>
      ((opt1: Option[FunctorLoci], s: Symbol, opt2: Option[FunctorLoci]) => FuncSymbolLoci(opt1,s,opt2)) |
      (leftFuncBracket ~ loci ~ rightFuncBracket) ~> ((left: Symbol, l: Loci, right: Symbol) => FuncBracketLoci(left,l,right)) }
  def functorProperty: Rule1[FunctorProperty] = rule{ ( "commutativity" | "idempotence" | "involutiveness" |
      "projectivity" ) ~ (justification ~ ";") ~> ((j: Justification) => FunctorProperty(j)) }
  def functorSynonym: Rule1[FunctorSynonym] = rule{ ("synonym" ~ functorPattern ~ "for" ~ functorPattern ~ ";") ~>
      ((f1: FunctorPattern, f2: FunctorPattern) => FunctorSynonym(f1,f2)) }
  def functorloci: Rule1[FunctorLoci] = rule{ locus | "(" ~ loci ~ ")" }

  def predDef: Rule1[PredicateDefinition] = rule{ ( "pred" ~ predicatePattern ~ optional("means" ~ definiens) ~
      ";" ~ correctConditions ~ zeroOrMore(predicateProperty) ) ~>
      ((patt: PredicatePattern, optD: Option[Definiens], cc: CorrectConditions, xs: Seq[PredicateProperty]) => PredicateDefinition(patt,optD,cc,xs.toList)) }
  def predicatePattern: Rule1[PredicatePattern] = rule{ (optional(loci) ~ predicateSymbol ~ optional(loci)) ~>
      ((opt1: Option[Loci], s: Symbol, opt2: Option[Loci]) => PredicatePattern(opt1,s,opt2)) }
  def predicateProperty: Rule1[PredicateProperty] = rule{ ("symmetry" | "asymmetry" | "connectedness" |
      "reflexivity" | "irreflexivity") ~ (justification ~ ";") ~> ((j: Justification) => PredicateProperty(j)) }
  def predicateSynonym: Rule1[PredicateSynonym] = rule{ ("synonym" ~ predicatePattern ~ "for" ~ predicatePattern ~ ";") ~>
      ((p1: PredicatePattern, p2: PredicatePattern) => PredicateSynonym(p1,p2)) }
  def predicateAntonym: Rule1[PredicateAntonym] = rule{ ("antonym" ~ predicatePattern ~ "for" ~ predicatePattern ~ ";") ~>
      ((p1: PredicatePattern, p2: PredicatePattern) => PredicateAntonym(p1,p2)) }

  def attrDef: Rule1[AttributeDefinition] = rule{ ("attr" ~ attributePattern ~ "means" ~ definiens ~ ";" ~ correctConditions) ~>
      ((patt: AttributePattern, d: Definiens, cc: CorrectConditions) => AttributeDefinition(patt,d,cc)) }
  def attributePattern: Rule1[AttributePattern] = rule{ (locus ~ "is" ~ optional(attributeLoci) ~ attributeSymbol) ~>
      ((l: Locus, optl: Option[AttributeLoci], s: Symbol) => AttributePattern(l,optl,s)) }
  def attributeSynonym: Rule1[AttributeSynonym] = rule{ ("synonym" ~ attributePattern ~ "for" ~ attributePattern ~ ";") ~>
      ((p1: AttributePattern, p2: AttributePattern) => AttributeSynonym(p1,p2)) }
  def attributeAntonym: Rule1[AttributeAntonym] = rule{ ("antonym" ~ attributePattern ~ "for" ~ attributePattern ~ ";") ~>
      ((p1: AttributePattern, p2: AttributePattern) => AttributeAntonym(p1,p2)) }
  def attributeLoci: Rule1[AttributeLoci] = rule{ ( loci | "(" ~ loci ~ ")" ) ~> ((l: Loci) => AttributeLoci(l)) }

  def clusterRegistration: Rule1[ClusterRegistration] = rule{ existentialRegis | conditionalRegis | functorialRegis }
  def existentialRegis: Rule1[ExistentialRegistration] = rule{ ( "cluster" ~ zeroOrMore(adjective) ~ "for" ~
      typeExpr ~ ";" ~ correctConditions ) ~> ((xs: Seq[Adjective], t: TypeExpression, cc: CorrectConditions) => ExistentialRegistration(xs.toList,t,cc)) }
  def adjective: Rule1[Adjective] = rule{ (optional("non") ~ optional(adjArguments) ~ attributeSymbol) ~>
      ((optA: Option[AdjectiveArguments], s: Symbol) => Adjective(optA,s)) }
  def conditionalRegis: Rule1[ConditionalRegistration] = rule{ ( "cluster" ~ zeroOrMore(adjective) ~ "->" ~ zeroOrMore(adjective) ~
      "for" ~ typeExpr ~ ";" ~ correctConditions ) ~> ((xs1: Seq[Adjective], xs2: Seq[Adjective], t: TypeExpression, cc: CorrectConditions) => ConditionalRegistration(xs1.toList,xs2.toList,t,cc)) }
  def functorialRegis: Rule1[FunctorialRegistration] = rule{ ( "cluster" ~ termExpr ~ "->" ~ zeroOrMore(adjective) ~
      optional("for" ~ typeExpr) ~ ";" ~ correctConditions ) ~> ((t: TermExpression, xs: Seq[Adjective], optT: Option[TypeExpression], cc: CorrectConditions) => FunctorialRegistration(t,xs.toList,optT,cc)) }

  def identifyRegistration: Rule1[IdentifyRegistration] = rule{ ( "identify" ~ functorPattern ~ "with" ~ functorPattern ~
      optional(locusStatement) ~ ";" ~ correctConditions ) ~>
      ((pat1: FunctorPattern, pat2: FunctorPattern, optL: Option[LocusStatement], cc: CorrectConditions) => IdentifyRegistration(pat1,pat2,optL,cc)) }
  def locusStatement: Rule1[LocusStatement] = rule{ "when" ~ oneOrMore(locusEqLocus).separatedBy(",") ~> ((xs: Seq[LocusEqLocus]) => LocusStatement(xs.toList)) }
  def locusEqLocus: Rule1[LocusEqLocus] = rule{ (locus ~ "=" ~ locus) ~> ((l1: Locus, l2: Locus) => LocusEqLocus(l1,l2)) }
  def propertyRegistration: Rule1[PropertyRegistration] = rule{ ("sethood" ~ "of" ~ typeExpr ~ justification ~ ";") ~>
      ((t: TypeExpression, j: Justification) => PropertyRegistration(t,j)) }
  def reductionRegistration: Rule1[ReductionRegistration] = rule{ ("reduce" ~ termExpr ~ "to" ~ termExpr ~
      ";" ~ correctConditions) ~> ((t1: TermExpression, t2: TermExpression, cc: CorrectConditions) => ReductionRegistration(t1,t2,cc)) }

  def correctConditions: Rule1[CorrectConditions] = rule{ (zeroOrMore(correctcond) ~ optional("correctness" ~ justification ~ ";")) ~>
      ((xs: Seq[CorrectCondition], optJ: Option[Justification]) => CorrectConditions(xs.toList,optJ)) }
  def correctcond = rule{ ("existence" | "uniqueness" | "coherence" | "compatibility" |
      "consistency" | "reducibility") ~ (justification ~ ";") ~> ((j: Justification) => CorrectCondition(j)) }

  def schemeBlock: Rule1[SchemeBlock] = rule{ (  "scheme" ~ identifier ~ "{" ~ schemeParameters ~ "}" ~ ":" ~
      sentence ~ optional(propList) ~ ("proof" | ";") ~ reasoning ~ "end" ) ~>
      ((i: Identifier, xs: Seq[SchemeSegment], s: Sentence, optP: Option[PropList], r: Reasoning) => SchemeBlock(i,xs.toList,s,optP,r)) }
  def propList: Rule1[PropList] = rule{ "provided" ~ oneOrMore(proposition).separatedBy("and") ~> ((xs: Seq[Proposition]) => PropList(xs.toList)) }
  def schemeParameters: Rule1[Seq[SchemeSegment]] = rule{ oneOrMore(schemeSeg).separatedBy(",") }
  def schemeSeg: Rule1[SchemeSegment] = rule{ predicateSeg | functorSeg }
  def predicateSeg: Rule1[PredicateSegment] = rule{ ( oneOrMore(identifier).separatedBy(",") ~ "[" ~
      optional(typExprList) ~ "]" ) ~> ((xs: Seq[Identifier], optT: Option[TypExpressionList]) => PredicateSegment(xs.toList,optT)) }
  def functorSeg: Rule1[FunctorSegment] = rule{ ( oneOrMore(identifier).separatedBy(",") ~ "(" ~
      optional(typExprList) ~ ")" ~ specification ) ~> ((xs: Seq[Identifier], optT: Option[TypExpressionList], s: Specification) => FunctorSegment(xs.toList,optT,s)) }
  def typExprList:Rule1[TypExpressionList] = rule{ oneOrMore(typeExpr).separatedBy(",") ~> ((xs: Seq[TypeExpression]) => TypExpressionList(xs.toList)) }

  def privateDefinition: Rule1[PrivateDefinition] = rule{ constantDef | privateFunctorDef | privatePredicateDef }
  def constantDef: Rule1[ConstantDefinition] = rule{ ("set" ~ oneOrMore(equating).separatedBy(",") ~ ";") ~> ((xs: Seq[Equating]) => ConstantDefinition(xs.toList)) }
  def equating: Rule1[Equating] = rule{ (variableIden ~ "=" ~ termExpr) ~> ((v: VariableIdentifier, t: TermExpression) => Equating(v,t)) }
  def privateFunctorDef: Rule1[PrivateFunctorDefinition] = rule{ ("deffunc" ~ privateFuncPattern ~ "=" ~ termExpr ~ ";") ~>
      ((p: PrivateFunctorPattern, t: TermExpression) => PrivateFunctorDefinition(p,t)) }
  def privatePredicateDef: Rule1[PrivatePredicateDefinition] = rule{ ("defpred" ~ privatePredPattern ~ "means" ~ sentence ~ ";") ~>
      ((p: PrivatePredicatePattern, s: Sentence) => PrivatePredicateDefinition(p,s)) }
  def privateFuncPattern: Rule1[PrivateFunctorPattern] = rule{ ( funcIdentifier ~ "(" ~ optional(typExprList) ~ ")" ) ~>
      ((f: FunctorIdentifier, optT: Option[TypExpressionList]) => PrivateFunctorPattern(f,optT)) }
  def privatePredPattern: Rule1[PrivatePredicatePattern] = rule{ (predIdentifier ~ "[" ~ optional(typExprList) ~ "]") ~>
      ((p: PredicateIdentifier, optT: Option[TypExpressionList]) => PrivatePredicatePattern(p,optT)) }
  def predIdentifier: Rule1[PredicateIdentifier] = rule{ identifier ~> ((i: Identifier) => PredicateIdentifier(i)) }
  def funcIdentifier: Rule1[FunctorIdentifier] = rule{ identifier ~> ((i: Identifier) => FunctorIdentifier(i)) }

  def reasoning: Rule1[Reasoning] = rule{ (zeroOrMore(reasoningItem) ~ optional(casesOrSuppose)) ~>
      ((xs: Seq[ReasoningItem], optCS: Option[CasesOrSuppose]) => Reasoning(xs.toList,optCS)) }
  def casesOrSuppose: Rule1[CasesOrSuppose] = rule{ optional("then") ~ "per" ~ "cases" ~ simplejust ~ ";" ~ (caseList ~>
      ((s: SimpleJustification, xs: Seq[Case]) => CaseList(s,xs.toList)) |
      supposeList ~> ((s: SimpleJustification, xs: Seq[Suppose]) => SupposeList(s,xs.toList))) }
  def caseList: Rule1[Seq[Case]] = rule{ oneOrMore(case_) }
  def case_ : Rule1[Case] = rule{ ("case" ~ propOrConds ~ ";" ~ reasoning ~ "end" ~ ";") ~> ((p: PropOrConds, r: Reasoning) => Case(p,r)) }
  def supposeList: Rule1[Seq[Suppose]] = rule{ oneOrMore(suppose) }
  def suppose: Rule1[Suppose] = rule{ ("suppose" ~ propOrConds ~ ";" ~ reasoning ~ "end" ~ ";") ~> ((p: PropOrConds, r: Reasoning) => Suppose(p,r)) }
  def propOrConds: Rule1[PropOrConds] = rule{ proposition | conditions }

  def reasoningItem: Rule1[ReasoningItem] = rule{ auxiliaryItem | skeletonItem }
  def skeletonItem: Rule1[SkeletonItem] = rule{ generalization | assumption | conclusion | exemplification }
  def generalization: Rule1[Generalization] = rule{ ("let" ~ qualifiedVars ~ optional("such" ~ conditions) ~ ";") ~>
      ((q: QualifiedVariables, optC: Option[Conditions]) => Generalization(q,optC)) }
  def assumption: Rule1[Assumption] = rule{ singleAssump | collectiveAssump | existentialAssump }
  def singleAssump: Rule1[SingleAssump] = rule{ ("assume" ~ proposition ~ ";") ~> ((p: Proposition) => SingleAssump(p)) }
  def collectiveAssump: Rule1[CollectiveAssump] = rule{ ("assume" ~ conditions ~ ";") ~> ((c: Conditions) => CollectiveAssump(c)) }
  def existentialAssump: Rule1[ExistentialAssump] = rule{ ("given" ~ qualifiedVars ~ optional("such" ~ conditions) ~ ";") ~>
      ((q: QualifiedVariables, optC: Option[Conditions]) => ExistentialAssump(q,optC)) }
  def conclusion: Rule1[Conclusion] = rule{ ("thus" | "hence") ~ (compactStatement | iterativeEquality) |
      diffuseConclusion }
  def diffuseConclusion: Rule1[DiffuseConclusion] = rule{ "thus" ~ diffuseStatement | "hereby" ~ reasoning ~ "end" ~ ";" }
  def exemplification: Rule1[Exemplification] = rule{ ("take" ~ oneOrMore(example).separatedBy(",") ~ ";") ~>
      ((xs: Seq[Example]) => Exemplification(xs.toList)) }
  def example: Rule1[Example] = rule{ termExpr ~> ((t: TermExpression) => TermExprExample(t)) | (variableIden ~ "=" ~ termExpr) ~>
      ((v: VariableIdentifier, t: TermExpression) => VarIdenAndTermExpr(v,t)) }

  def statement: Rule1[Statement] = rule{ optional("then") ~ linkableStatement | diffuseStatement }
  def linkableStatement: Rule1[LinkableStatement] = rule{ compactStatement | choiceStatement | typechangingStatement |
      iterativeEquality }
  def compactStatement: Rule1[CompactStatement] = rule{ (proposition ~ justification ~ ";") ~> ((p: Proposition, j: Justification) => CompactStatement(p,j)) }
  def choiceStatement: Rule1[ChoiceStatement] = rule{ ("consider" ~ qualifiedVars ~ "such" ~ conditions ~ simplejust ~ ";") ~>
      ((q: QualifiedVariables, c: Conditions, s: SimpleJustification) => ChoiceStatement(q,c,s)) }
  def typechangingStatement: Rule1[TypeChangingStatement] = rule{ ("reconsider" ~ typeChangeList ~ "as" ~ typeExpr ~ simplejust ~ ";") ~>
      ((tl: TypeChangeList, t: TypeExpression, s: SimpleJustification) => TypeChangingStatement(tl,t,s)) }
  def typeChangeList: Rule1[TypeChangeList] = rule{ oneOrMore(eqOrVarIden).separatedBy(",") ~> ((xs: Seq[EquatingOrVarIdent]) => TypeChangeList(xs.toList)) }
  def eqOrVarIden: Rule1[EquatingOrVarIdent] = rule{ equating | variableIden }

  def iterativeEquality: Rule1[IterativeEquality] = rule{ ( optional(identifier ~ ":") ~ termExpr ~ "=" ~ termAndJust ~ oneOrMore(eqTermAndJust) ~ ";" ) ~>
      ((optI: Option[Identifier], t: TermExpression, tj: TermAndJustification, xs: Seq[TermAndJustification]) => IterativeEquality(optI,t,tj,xs.toList)) }
  def termAndJust: Rule1[TermAndJustification] = rule{ (termExpr ~ simplejust) ~> ((t: TermExpression, s: SimpleJustification) => TermAndJustification(t,s)) }
  def eqTermAndJust: Rule1[TermAndJustification] = rule{ (".=" ~ termExpr ~ simplejust) ~> ((t: TermExpression, s: SimpleJustification) => TermAndJustification(t,s)) }
  def diffuseStatement: Rule1[DiffuseStatement] = rule{ (optional(identifier ~ ":") ~ "now" ~ reasoning ~ "end" ~ ";") ~>
      ((optI: Option[Identifier], r: Reasoning) => DiffuseStatement(optI,r)) }
  def justification: Rule1[Justification] = rule{ simplejust | proof }
  def simplejust: Rule1[SimpleJustification] = rule{ straightforwardJust | schemeJust }
  def proof: Rule1[Proof] = rule{ ("proof" ~ reasoning ~ "end") ~> ((r: Reasoning) => Proof(r)) }
  def schemeJust: Rule1[SchemeJustification] = rule{ ( "from" ~ schReference ~ optional("(" ~ references ~ ")") ) ~>
      ((sr: SchemeReference, optR: Option[References]) => SchemeJustification(sr,optR)) }
  def straightforwardJust: Rule1[StraightforwardJustification] = rule{ optional("by" ~ references) ~>
      ((optR: Option[References]) => StraightforwardJustification(optR)) }
  def references: Rule1[References] = rule{ oneOrMore(reference).separatedBy(",") ~> ((xs: Seq[Reference]) => References(xs.toList)) }
  def reference: Rule1[Reference] = rule{ localReference | libraryReference }
  def schReference: Rule1[SchemeReference] = rule{ localSchemeReference | librarySchemeReference }
  def localReference: Rule1[LocalReference] = rule{ identifier ~> ((i: Identifier) => LocalReference(i)) }
  def localSchemeReference: Rule1[LocalSchemeReference] = rule{ identifier ~> ((i: Identifier) => LocalSchemeReference(i)) }
  def libraryReference: Rule1[LibraryReference] = rule{ (filename ~ ":" ~ oneOrMore(thmdefNum).separatedBy(",")) ~>
      ((f: FileName, xs: Seq[ThmDefNum]) => LibraryReference(f,xs.toList)) }
  def thmdefNum: Rule1[ThmDefNum] = rule{ thmNum | "def" ~ defNum }
  def librarySchemeReference: Rule1[LibrarySchemeReference] = rule{ (filename ~ ":" ~ "sch" ~ schNum) ~> ((f: FileName, s: SchemeNum) => LibrarySchemeReference(f,s)) }
  def thmNum: Rule1[TheoremNumber] = rule{ numeral ~> ((n: Numeral) => TheoremNumber(n)) }
  def defNum: Rule1[DefNumber] = rule{ numeral ~> ((n: Numeral) => DefNumber(n)) }
  def schNum: Rule1[SchemeNum] = rule{ numeral ~> ((n: Numeral) => SchemeNum(n)) }

  def conditions: Rule1[Conditions] = rule{ "that" ~ oneOrMore(proposition).separatedBy("and") ~> ((xs: Seq[Proposition]) => Conditions(xs.toList)) }
  def proposition: Rule1[Proposition] = rule{ (optional(identifier ~ ":") ~ sentence) ~> ((optI: Option[Identifier], s: Sentence) => Proposition(optI,s)) }
  def sentence: Rule1[Sentence] = rule{ formulaExpr ~> ((f: FormulaExpression) => Sentence(f)) }



/////////////////////////////   Expressions   /////////////////////////////////
  sealed trait FormulaExpr0
  case class ConjunctedFormExpr0(f: FormulaExpression, optf0: Option[FormulaExpr0]) extends FormulaExpr0
  case class MultiConjunctedFormExpr0(f: FormulaExpression, optf0: Option[FormulaExpr0]) extends FormulaExpr0
  case class DisjunctedFormExpr0(f: FormulaExpression, optf0: Option[FormulaExpr0]) extends FormulaExpr0
  case class MultiDisjunctedFormExpr0(f: FormulaExpression, optf0: Option[FormulaExpr0]) extends FormulaExpr0
  case class ImplicativeFormExpr0(f: FormulaExpression, optf0: Option[FormulaExpr0]) extends FormulaExpr0
  case class IffFormExpr0(f: FormulaExpression, optf0: Option[FormulaExpr0]) extends FormulaExpr0


  def formulaExpr: Rule1[FormulaExpression] = rule{ ( "(" ~ formulaExpr ~ ")" ~ optional(formulaExpr0) |
      atomicFormulaExpr ~ optional(formulaExpr0) | quantifiedFormulaExpr ~ optional(formulaExpr0) |
      "not" ~ formulaExpr ~ optional(formulaExpr0) | contradiction ~ optional(formulaExpr0) | thesis ~ optional(formulaExpr0) ) ~>
      ((f1: FormulaExpression, optF: Option[FormulaExpr0]) => optF.getOrElse(Nil) match {
        case Nil => f1
        case ConjunctedFormExpr0(f,optf0) => ConjunctedFormExpr(f1,f)
        case MultiConjunctedFormExpr0(f,optf0) => MultiConjunctedFormExpr(f1,f)
        case DisjunctedFormExpr0(f,optf0) => DisjunctedFormExpr(f1,f)
        case MultiDisjunctedFormExpr0(f,optf0) => MultiConjunctedFormExpr(f1,f)
        case ImplicativeFormExpr0(f,optf0) => ImplicativeFormExpr(f1,f)
        case IffFormExpr0(f,optf0) => IffFormExpr(f1,f)
      }
      ) }

  def formulaExpr0: Rule1[FormulaExpr0] = rule{ ("&" ~ formulaExpr ~ optional(formulaExpr0)) ~> ((f: FormulaExpression, optf0: Option[FormulaExpr0]) => ConjunctedFormExpr0(f,optf0)) |
      ("&" ~ "..." ~ "&" ~ formulaExpr ~ optional(formulaExpr0)) ~> ((f: FormulaExpression, optf0: Option[FormulaExpr0]) => MultiConjunctedFormExpr0(f,optf0)) |
      ("or" ~ formulaExpr ~ optional(formulaExpr0)) ~> ((f: FormulaExpression, optf0: Option[FormulaExpr0]) => DisjunctedFormExpr0(f,optf0)) |
      ("or" ~ "..." ~ "or" ~ formulaExpr ~ optional(formulaExpr0)) ~> ((f: FormulaExpression, optf0: Option[FormulaExpr0]) => MultiDisjunctedFormExpr0(f,optf0)) |
      ("implies" ~ formulaExpr ~ optional(formulaExpr0)) ~> ((f: FormulaExpression, optf0: Option[FormulaExpr0]) => ImplicativeFormExpr0(f,optf0)) |
      ("iff" ~ formulaExpr ~ optional(formulaExpr0)) ~> ((f: FormulaExpression, optf0: Option[FormulaExpr0]) => IffFormExpr0(f,optf0)) }

  def contradiction: Rule1[FormulaExpression] = rule{ "contradiction" ~ push(Contradiction) }
  def thesis: Rule1[FormulaExpression] = rule{ "thesis" ~ push(Thesis) }

  def atomicFormulaExpr: Rule1[AtomicFormulaExpression] = rule{ ( optional(termExprList) ~ optional(("does"|"do") ~ "not") ~ predicateSymbol ~ optional(termExprList) ~
      zeroOrMore(predSymAndTermExps) ) ~>
      ((opt1: Option[TermExpressionList], s: Symbol, opt2: Option[TermExpressionList], xs: Seq[PredSymAndTermExps]) => BunchOfTermExpAndSymbol(opt1,s,opt2,xs.toList)) |
      (predIdentifier ~ "[" ~ optional(termExprList) ~ "]") ~> ((i: PredicateIdentifier, optT: Option[TermExpressionList]) => PredIdenTermExps(i,optT)) |
      (termExpr ~ "is" ~ oneOrMore(adjective)) ~> ((t: TermExpression, xs: Seq[Adjective]) => TermExpIsAjectives(t,xs.toList)) |
      (termExpr ~ "is" ~ typeExpr) ~> ((t: TermExpression, ty: TypeExpression) => TermExpIsTypeExp(t,ty)) }

  def predSymAndTermExps: Rule1[PredSymAndTermExps] = rule{ (optional(("does"|"do") ~ "not") ~ predicateSymbol ~ termExprList) ~>
      ((p: Symbol, t: TermExpressionList) => PredSymAndTermExps(p,t)) }

  def quantifiedFormulaExpr: Rule1[QuantifiedFormulaExpression] = rule{ ( "for" ~ qualifiedVars ~ optional("st" ~ formulaExpr) ~
      formOrquanExp ) ~> ((q: QualifiedVariables, optF: Option[FormulaExpression], fORq: FormulaOrQuantified) => BunchOfQualVarsAndExps(q,optF,fORq)) |
      ("ex" ~ qualifiedVars ~ "st" ~ formulaExpr) ~> ((q: QualifiedVariables, f: FormulaExpression) => QualVarsAndFormExp(q,f)) }
  def formOrquanExp: Rule1[FormulaOrQuantified] = rule{ "holds" ~ formulaExpr ~> ((f: FormulaExpression) => Formula(f)) |
      quantifiedFormulaExpr ~> ((q: QuantifiedFormulaExpression) => Quantified(q)) }

  def qualifiedVars: Rule1[QualifiedVariables] = rule{ (explicitlyQualVars ~ "," ~ implicitlyQualVars) ~>
      ((e: ExpQualVariables, i: ImpQualVariables) => BothQualVariables(e,i)) |
      explicitlyQualVars | implicitlyQualVars }
  def implicitlyQualVars: Rule1[ImpQualVariables] = rule{ oneOrMore(variableIden).separatedBy(",") ~>
      ((xs: Seq[VariableIdentifier]) => ImpQualVariables(xs.toList)) }
  def explicitlyQualVars: Rule1[ExpQualVariables] = rule{ oneOrMore(qualifiedSegment).separatedBy(",") ~>
      ((xs: Seq[QualifiedSegment]) => ExpQualVariables(xs.toList)) }
  def qualifiedSegment: Rule1[QualifiedSegment] = rule{ ( oneOrMore(variableIden).separatedBy(",") ~ qualification ) ~>
      ((xs: Seq[VariableIdentifier], q: Qualification) => QualifiedSegment(xs.toList,q)) }
  def qualification: Rule1[Qualification] = rule{ ("being" | "be") ~ typeExpr ~> ((ty: TypeExpression) => Qualification(ty)) }

  def typeExpr: Rule1[TypeExpression] = rule{ "(" ~ radixType ~ ")" |
      //zeroOrMore(adjective) ~ typeExpr |
      (oneOrMore(adjective) ~ typeExpr) ~> ((xs: Seq[Adjective], ty: TypeExpression) => AdjClusterAndTypeExp(xs.toList,ty)) |
      radixType }

  def structTypeExp: Rule1[StructureTypeExpression] = rule{ "(" ~ structSymbol ~ optional("over" ~ termExprList) ~ ")" ~>
      ((s: Symbol, optT: Option[TermExpressionList]) => WithoutAdjectives(s,optT)) |
      ( zeroOrMore(adjective) ~ structSymbol ~ optional("over" ~ termExprList) ) ~>
      ((xs: Seq[Adjective], s: Symbol, optT: Option[TermExpressionList]) => WithAdjectives(xs.toList,s,optT)) }
  def radixType: Rule1[RadixType] = rule{ (modeSymbol ~ optional("of" ~ termExprList)) ~>
      ((s: Symbol, optT: Option[TermExpressionList]) => ModeSymOptExps(s,optT)) |
      (structSymbol ~ optional("over" ~ termExprList)) ~>
      ((s: Symbol, optT: Option[TermExpressionList]) => StructSymOptExps(s,optT)) }

  def termExpr: Rule1[TermExpression] = rule{ (
     "it" ~ push(It) ~ optional(termExpr0) |
      "the" ~ selectorSymbol ~> ((s: Symbol) => SelectorSymbol(s)) ~ optional(termExpr0) |
      numeral ~> ((n: Numeral) => NumTerm(n)) ~ optional(termExpr0) |
      variableIden ~ optional(termExpr0) |
      privateDefParameter ~ optional(termExpr0) |
      "(" ~ termExpr ~ ")" ~ optional(termExpr0) |
      (leftFuncBracket ~ termExprList ~ rightFuncBracket) ~> ((l: Symbol, t: TermExpressionList, r: Symbol) => FuncBracketedTermExps(l,t,r)) ~ optional(termExpr0) |
      (funcIdentifier ~ "(" ~ optional(termExprList) ~ ")") ~> ((f: FunctorIdentifier, optT: Option[TermExpressionList]) => FuncIdenExpList(f,optT)) ~ optional(termExpr0) |
      (structSymbol ~ "(#" ~ termExprList ~ "#)") ~> ((s: Symbol, t: TermExpressionList) => StructSymExps(s,t)) ~ optional(termExpr0) |
      ("the" ~ structSymbol ~ "of" ~ termExpr) ~> ((s: Symbol, t: TermExpression) => StructSymExp(s,t)) ~ optional(termExpr0) |
      ("{" ~ termExpr ~ zeroOrMore(postqualification) ~ ":" ~ sentence ~ "}") ~> ((t: TermExpression, xs: Seq[Postqualification], s: Sentence) => WithSentence(t,xs.toList,s)) ~ optional(termExpr0) |
      ("the" ~ "set" ~ "of" ~ "all" ~ termExpr ~ zeroOrMore(postqualification)) ~> ((t: TermExpression, xs: Seq[Postqualification]) => WithoutSentence(t,xs.toList)) ~ optional(termExpr0) |
      //termExpr ~ "qua" ~ typeExpr |
      ("the" ~ selectorSymbol ~ "of" ~ termExpr) ~> ((s: Symbol, t: TermExpression) => SelectorSymExp(s,t)) ~ optional(termExpr0) |
      "the" ~ typeExpr ~ optional(termExpr0) |
      //optional(arguments) ~ symbol ~ optional(arguments) |
      (funcSymbol ~ optional(arguments)) ~> ((f: Symbol, optA: Option[Arguments]) => ArguFuncSymbol(None,f,optA)) ~ optional(termExpr0) ) ~>
      ((t1: TermExpression, optt0: Option[TermExpr0]) => optt0.getOrElse(Nil) match {
        case Nil => t1
        case TermAndTypeExp0(ty,_) => TermAndTypeExp(t1,ty)
        case ArguFuncSymbol0(s,optA,_) => ArguFuncSymbol(Some(t1),s,optA)
      }
      ) }

  sealed trait TermExpr0
  case class TermAndTypeExp0(ty: TypeExpression, optt0: Option[TermExpr0]) extends TermExpr0
  case class ArguFuncSymbol0(s: Symbol, optA: Option[Arguments], optt0: Option[TermExpr0]) extends TermExpr0


  def termExpr0: Rule1[TermExpr0] = rule{ ("qua" ~ typeExpr ~ optional(termExpr0)) ~>
      ((ty: TypeExpression, optt0: Option[TermExpr0]) => TermAndTypeExp0(ty,optt0)) |
      ( funcSymbol ~ optional(arguments) ~ optional(termExpr0) ) ~>
      ((s: Symbol, optA: Option[Arguments], optt0: Option[TermExpr0]) => ArguFuncSymbol0(s,optA,optt0)) }

  def termExprList: Rule1[TermExpressionList] = rule{ oneOrMore(termExpr).separatedBy(",") ~> ((xs: Seq[TermExpression]) => TermExpressionList(xs.toList)) }

  def arguments: Rule1[Arguments] = rule{ "(" ~ termExprList ~ ")" | termExpr }
  def adjArguments: Rule1[AdjectiveArguments] = rule{ ( "(" ~ termExprList ~ ")" | termExprList ) ~> ((t: TermExpressionList) => AdjectiveArguments(t)) }

  def postqualification: Rule1[Postqualification] = rule{ "where" ~ oneOrMore(postqualSegment).separatedBy(",") ~> ((xs: Seq[PostqualiSegment]) => Postqualification(xs.toList)) }
  def postqualSegment: Rule1[PostqualiSegment] = rule{ ( oneOrMore(identifier).separatedBy(",") ~ optional(("is"|"are") ~ typeExpr) ) ~>
      ((xs: Seq[Identifier], optTy: Option[TypeExpression]) => PostqualiSegment(xs.toList,optTy)) }

  def privateDefParameter: Rule1[PrivateDefinitionParameter] = rule{ "$10" ~ push(Para10) | "$1" ~ push(Para1) | "$2" ~ push(Para2) | "$3" ~ push(Para3) | "$4" ~ push(Para4) |
     "$5" ~ push(Para5) | "$6" ~ push(Para6) | "$7" ~ push(Para7) | "$8" ~ push(Para8) | "$9" ~ push(Para9) }

}
