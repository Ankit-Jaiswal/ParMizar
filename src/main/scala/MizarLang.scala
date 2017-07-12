package ParMizar

object MizarLang {

  case class Identifier(name: String)
  case class FileName(name: String)
  case class Symbol(name: String)
  case class Numeral(name: String)

  case class Article(environ: EnvironmentDeclaration, text: TextProper)


////////////////////////        Environment         ////////////////////////
  case class EnvironmentDeclaration(directives: List[Directive])

  sealed trait Directive{
      val files : List[FileName]
  }

  case class VocabularyDirective(files: List[FileName]) extends Directive
  case class RequirementDirective(files: List[FileName]) extends Directive
  sealed trait LibraryDirective extends Directive

  case class Notations(files: List[FileName]) extends LibraryDirective
  case class Constructors(files: List[FileName]) extends LibraryDirective
  case class Registrations(files: List[FileName]) extends LibraryDirective
  case class Expansions(files: List[FileName]) extends LibraryDirective
  case class Equalities(files: List[FileName]) extends LibraryDirective
  case class Definitions(files: List[FileName]) extends LibraryDirective
  case class Theorems(files: List[FileName]) extends LibraryDirective
  case class Schemes(files: List[FileName]) extends LibraryDirective




/////////////////////////     TextProper  ////////////////////////////////
  case class TextProper(sections : List[Section]) extends AnyVal

  case class Section(texts: List[TextItem])

  sealed trait TextItem
  case class Reservation(segments: List[ReservationSegment]) extends TextItem
  case class DefinitionalItem(defList: List[DefinitionalBlock]) extends TextItem
  case class RegistrationItem(regisList: List[RegistrationBlock]) extends TextItem
  case class NotationItem(noteList: List[NotationBlock]) extends TextItem
  case class Theorem(thm: CompactStatement) extends TextItem
  case class SchemeItem(schblock: SchemeBlock) extends TextItem
  sealed trait AuxiliaryItem extends RegistrationBlock with DefinitionItem
        with ReasoningItem with TextItem


//// 1st layer expansion
  case class ReservationSegment(reservedIden: ReservedIdentifiers,
                                typ : TypeExpression)
  case class ReservedIdentifiers(idents: List[Identifier])

  sealed trait DefinitionalBlock
  sealed trait DefinitionItem extends DefinitionalBlock
  sealed trait Definition extends DefinitionalBlock
  sealed trait Redefinition extends DefinitionalBlock


  sealed trait RegistrationBlock
  sealed trait ClusterRegistration extends RegistrationBlock
  case class IdentifyRegistration(identifyPatt: FunctorPattern,
        withPatt: FunctorPattern, locusState: Option[LocusStatement],
        corrCond: CorrectConditions) extends RegistrationBlock
  case class PropertyRegistration(typ: TypeExpression, just: Justification)
        extends RegistrationBlock
  case class ReductionRegistration(reduceTerm: TermExpression,
        toTerm: TermExpression, corrCond: CorrectConditions)
        extends RegistrationBlock


  sealed trait NotationBlock
  case class LociDeclaration(qualVar: QualifiedVariables,
        optCond: Option[Conditions]) extends RegistrationBlock with NotationBlock
        with DefinitionItem
  sealed trait NotationDeclaration extends NotationBlock

  case class SchemeBlock(schemeId: Identifier, schParameters: List[SchemeSegment],
    schConcl: Sentence, schPremise: Option[PropList], reason: Reasoning)
  case class PropList(list: List[Proposition])



//// 2nd layer expansion
  case class PermissiveAssumption(assump: Assumption) extends DefinitionItem


  case class StructureDefinition(optAncestors: Option[Ancestors],
        structSym: Symbol, optLoci: Option[Loci],
        fields: List[FieldSegment] ) extends Definition
  case class Ancestors(exprList: List[StructureTypeExpression])
  case class ModeDefinition(modePatt: ModePattern, modeBlock: ModeBlock,
        modeProp: List[ModeProperty] ) extends Definition with Redefinition
  case class FunctorDefinition(funPatt: FunctorPattern,
        optSpec: Option[Specification], optDefiens: Option[Definiens],
        corrCond: CorrectConditions, funcProp: List[FunctorProperty])
        extends Definition with Redefinition
  case class PredicateDefinition(predPatt: PredicatePattern,
        optDefiens: Option[Definiens], corrCond: CorrectConditions,
        predProp: List[PredicateProperty]) extends Definition with Redefinition
  case class AttributeDefinition(attPatt: AttributePattern, defiens: Definiens,
        corrCond: CorrectConditions) extends Definition with Redefinition


  case class ExistentialRegistration(adjList: List[Adjective],
        typ: TypeExpression, corrCond: CorrectConditions)
        extends ClusterRegistration
  case class ConditionalRegistration(adjList: List[Adjective],
        toAdjList: List[Adjective], typ: TypeExpression, corrCond: CorrectConditions)
        extends ClusterRegistration
  case class FunctorialRegistration(termExp: TermExpression, toAdjList: List[Adjective],
        optTyp: Option[TypeExpression], corrCond: CorrectConditions)
        extends ClusterRegistration


  case class LocusStatement(locusequality: List[LocusEqLocus])
  case class LocusEqLocus(locus: Locus, eqlocus: Locus)


  sealed trait Statement extends AuxiliaryItem
  sealed trait PrivateDefinition extends AuxiliaryItem


  case class AttributeSynonym(patt: AttributePattern, forPatt: AttributePattern)
        extends NotationDeclaration
  case class AttributeAntonym(patt: AttributePattern, forPatt: AttributePattern)
        extends NotationDeclaration
  case class FunctorSynonym(patt: FunctorPattern, forPatt: FunctorPattern)
        extends NotationDeclaration
  case class ModeSynonym(patt: ModePattern, forPatt: ModePattern)
        extends NotationDeclaration
  case class PredicateSynonym(patt: PredicatePattern, forPatt: PredicatePattern)
        extends NotationDeclaration
  case class PredicateAntonym(patt: PredicatePattern, forPatt: PredicatePattern)
        extends NotationDeclaration


  case class Proposition(optLabel: Option[Identifier], sent: Sentence) extends PropOrConds
  sealed trait Justification
  sealed trait SimpleJustification extends Justification
  case class Proof(pf: Reasoning) extends Justification


  sealed trait SchemeSegment
  case class PredicateSegment(predId: List[Identifier],
        optTypList: Option[TypExpressionList]) extends SchemeSegment
  case class FunctorSegment(funcId: List[Identifier],
        optTypList: Option[TypExpressionList], spec: Specification)
        extends SchemeSegment

  case class Reasoning(reasItems: List[ReasoningItem],
        casesOrSuppose: Option[CasesOrSuppose]) extends DiffuseConclusion
  sealed trait CasesOrSuppose{
    val just : SimpleJustification
  }
  case class CaseList(just: SimpleJustification, cases: List[Case])
        extends CasesOrSuppose
  case class SupposeList(just: SimpleJustification, supps: List[Suppose])
        extends CasesOrSuppose


///// 3rd layer expansion
  sealed trait Assumption extends SkeletonItem
  case class SingleAssump(prop: Proposition) extends Assumption
  case class CollectiveAssump(conds: Conditions) extends Assumption
  case class ExistentialAssump(qualVar: QualifiedVariables, optCond: Option[Conditions])
        extends Assumption

  case class Locus(varIden: VariableIdentifier) extends FunctorLoci

  case class FieldSegment(selectSym: List[Symbol], spec: Specification)

  case class ModePattern(modeSym: Symbol, optloci: Option[Loci])

  sealed trait ModeBlock
  case class ModeCondBlock(optSpec: Option[Specification], optDefinien: Option[Definiens],
        corrCond: CorrectConditions) extends ModeBlock
  case class ModeTypExpBlock(typExp: TypeExpression) extends ModeBlock
  case class CorrectConditions(condList: List[CorrectCondition],
        optjust: Option[Justification])
  case class ModeProperty(just: Justification)

  sealed trait FunctorPattern
  case class FuncSymbolLoci(opt1: Option[FunctorLoci], funcSym: Symbol,
        opt2: Option[FunctorLoci]) extends FunctorPattern
  case class FuncBracketLoci(lBracket: Symbol, loci: Loci,
        rBracket: Symbol) extends FunctorPattern

  case class Specification(typExp: TypeExpression)

  sealed trait Definiens
  case class SimpleDefiniens(optLabel: Option[Identifier], sentExp: SentOrTerm)
        extends Definiens
  case class ConditionalDefiniens(optLabel: Option[Identifier],
        partialList: List[PartialDefiniens], optSentExp: Option[SentOrTerm])
        extends Definiens

  case class CorrectCondition(just: Justification)

  case class FunctorProperty(just: Justification)

  case class PredicatePattern(opt1loci: Option[Loci], predSym: Symbol,
        opt2loci: Option[Loci])

  case class PredicateProperty(just: Justification)

  case class AttributePattern(locus: Locus, optloci: Option[AttributeLoci],
        attSym: Symbol)
  case class AttributeLoci(loci: Loci)

  case class Adjective(optArg: Option[AdjectiveArguments], attSym: Symbol)

  case class DiffuseStatement(optLabel: Option[Identifier], reason: Reasoning)
        extends Statement with DiffuseConclusion
  sealed trait LinkableStatement extends Statement

  case class CompactStatement(prop: Proposition, just: Justification)
        extends LinkableStatement with Conclusion
  case class ChoiceStatement(qualVar: QualifiedVariables, conds: Conditions,
        just: SimpleJustification) extends LinkableStatement
  case class TypeChangingStatement(changeList: TypeChangeList,
        typExp: TypeExpression, just: SimpleJustification) extends LinkableStatement
  case class IterativeEquality(optLabel: Option[Identifier], termExp: TermExpression, eqTermNJust: TermAndJustification,
        equalsTerm: List[TermAndJustification]) extends LinkableStatement
        with Conclusion
  case class TermAndJustification(term: TermExpression, just: SimpleJustification)


  case class ConstantDefinition(eqList: List[Equating]) extends PrivateDefinition
  case class PrivateFunctorDefinition(patt: PrivateFunctorPattern,
        term: TermExpression) extends PrivateDefinition
  case class PrivatePredicateDefinition(patt: PrivatePredicatePattern,
        sent: Sentence) extends PrivateDefinition


  case class StraightforwardJustification(optReferences: Option[References])
        extends SimpleJustification
  case class SchemeJustification(schRefer: SchemeReference,
        optReferences: Option[References]) extends SimpleJustification

  case class Case(prop: PropOrConds, reason: Reasoning)
  case class Suppose(prop: PropOrConds, reason: Reasoning)
  sealed trait PropOrConds


//// 4th layer expansion
  case class Conditions(props: List[Proposition]) extends PropOrConds

  sealed trait FunctorLoci
  case class Loci(loci: List[Locus]) extends FunctorLoci

  sealed trait SentOrTerm
  case class Sentence(expr: FormulaExpression) extends SentOrTerm
  case class DefienTerm(term: TermExpression) extends SentOrTerm

  case class PartialDefiniens(sentExp: SentOrTerm, sent: Sentence)

  case class TypeChangeList(eqVarList: List[EquatingOrVarIdent])
  sealed trait EquatingOrVarIdent
  case class Equating(varIden: VariableIdentifier, equalsTerm: TermExpression)
        extends EquatingOrVarIdent
  case class VariableIdentifier(iden: Identifier) extends EquatingOrVarIdent
        with TermExpression

  case class PrivateFunctorPattern(funcIden: FunctorIdentifier,
        optExp: Option[TypExpressionList])
  case class FunctorIdentifier(iden: Identifier)

  case class PrivatePredicatePattern(predIden: PredicateIdentifier,
        optExp: Option[TypExpressionList])
  case class PredicateIdentifier(iden: Identifier)


  case class References(refers: List[Reference])
  sealed trait Reference
  case class LocalReference(label: Identifier) extends Reference
  case class LibraryReference(articleName: FileName, index: List[ThmDefNum]) extends Reference

  sealed trait ThmDefNum
  case class TheoremNumber(num: Numeral) extends ThmDefNum
  case class DefNumber(num: Numeral) extends ThmDefNum

  sealed trait SchemeReference
  case class LocalSchemeReference(schIden: Identifier) extends SchemeReference
  case class LibrarySchemeReference(articleName: FileName, schNum: SchemeNum)
        extends SchemeReference
  case class SchemeNum(num: Numeral)



///// 5th layer expansion
  sealed trait ReasoningItem
  sealed trait SkeletonItem extends ReasoningItem

  case class Generalization(qualVar: QualifiedVariables, optConds: Option[Conditions])
        extends SkeletonItem
  sealed trait Conclusion extends SkeletonItem
  case class Exemplification(examples: List[Example]) extends SkeletonItem

  sealed trait DiffuseConclusion extends Conclusion

  sealed trait Example
  case class TermExprExample(termExp: TermExpression) extends Example
  case class VarIdenAndTermExpr(varIden: VariableIdentifier, termExp: TermExpression)
        extends Example





///////////////////////       Expressions       //////////////////////////////
  sealed trait FormulaExpression
  case object Contradiction extends FormulaExpression
  case object Thesis extends FormulaExpression
  case class BracketedFormExpr(formExpr: FormulaExpression) extends FormulaExpression
  sealed trait AtomicFormulaExpression extends FormulaExpression
  sealed trait QuantifiedFormulaExpression extends FormulaExpression
  case class ConjunctedFormExpr(firstExpr: FormulaExpression,
        secondExpr: FormulaExpression) extends FormulaExpression
  case class MultiConjunctedFormExpr(firstExpr: FormulaExpression,
        lastExpr: FormulaExpression) extends FormulaExpression
  case class DisjunctedFormExpr(firstExpr: FormulaExpression,
        secondExpr: FormulaExpression) extends FormulaExpression
  case class MultiDisjunctedFormExpr(firstExpr: FormulaExpression,
        lastExpr: FormulaExpression) extends FormulaExpression
  case class ImplicativeFormExpr(antecedent: FormulaExpression,
        consequent: FormulaExpression) extends FormulaExpression
  case class IffFormExpr(firstExpr: FormulaExpression,
        secondExpr: FormulaExpression) extends FormulaExpression
  case class NegativeFormExpr(formExpr: FormulaExpression) extends FormulaExpression


  case class BunchOfTermExpAndSymbol(firstOptExps: Option[TermExpressionList],
        predSym: Symbol, secondOptExps: Option[TermExpressionList],
        symExpsList: List[PredSymAndTermExps]) extends AtomicFormulaExpression
  case class PredSymAndTermExps(predSym: Symbol, termExps: TermExpressionList)
  case class PredIdenTermExps(iden: PredicateIdentifier,
        optTermExps: Option[TermExpressionList]) extends AtomicFormulaExpression
  case class TermExpIsAjectives(termExp: TermExpression, adjList: List[Adjective])
        extends AtomicFormulaExpression
  case class TermExpIsTypeExp(termExp: TermExpression, typExp: TypeExpression)
        extends AtomicFormulaExpression


  case class QualVarsAndFormExp(qualVars: QualifiedVariables, formExpr: FormulaExpression)
        extends QuantifiedFormulaExpression
  case class BunchOfQualVarsAndExps(qualVars: QualifiedVariables,
        optFormExp: Option[FormulaExpression], formOrquanExp: FormulaOrQuantified)
        extends QuantifiedFormulaExpression
  sealed trait FormulaOrQuantified
  case class Formula(formExpr: FormulaExpression) extends FormulaOrQuantified
  case class Quantified(quanExpr: QuantifiedFormulaExpression) extends FormulaOrQuantified


  sealed trait QualifiedVariables
  case class ImpQualVariables(vars: List[VariableIdentifier]) extends QualifiedVariables
  case class ExpQualVariables(qualSegs: List[QualifiedSegment])
        extends QualifiedVariables
  case class BothQualVariables(expVar: ExpQualVariables, impVar: ImpQualVariables)
        extends QualifiedVariables


  case class QualifiedSegment(vars: List[VariableIdentifier], qual: Qualification)
  case class Qualification(typExp: TypeExpression)


  case class Variables(varIdenList: List[VariableIdentifier])


  sealed trait TypeExpression extends TermExpression
  case class AdjClusterAndTypeExp(adjcluster: List[Adjective], typExp: TypeExpression)
        extends TypeExpression
  sealed trait RadixType extends TypeExpression
  case class TypExpressionList(list: List[TypeExpression])

  case class ModeSymOptExps(modeSym: Symbol, optTermExps: Option[TermExpressionList])
        extends RadixType
  case class StructSymOptExps(structSym: Symbol, optTermExps: Option[TermExpressionList])
        extends RadixType


  sealed trait StructureTypeExpression
  case class WithAdjectives(adjcluster: List[Adjective], structSym: Symbol,
        optTermExps: Option[TermExpressionList]) extends StructureTypeExpression
  case class WithoutAdjectives(structSym: Symbol, optTermExps: Option[TermExpressionList])
        extends StructureTypeExpression


  sealed trait TermExpression extends Arguments
  case class BracketedTermExp(termExp: TermExpression) extends TermExpression
  case class ArguFuncSymbol(firstOptArgs: Option[Arguments], funcSym: Symbol,
        secondOptArgs: Option[Arguments]) extends TermExpression
  case class FuncBracketedTermExps(left: Symbol, termExps: TermExpressionList,
        right: Symbol) extends TermExpression
  case class FuncIdenExpList(funcId: FunctorIdentifier,
        optTermExps: Option[TermExpressionList]) extends TermExpression
  case class StructSymExps(structSym: Symbol, termExps: TermExpressionList)
        extends TermExpression
  case class StructSymExp(structSym: Symbol, termExp: TermExpression)
        extends TermExpression
  case class WithSentence(termExp: TermExpression, postquals: List[Postqualification],
        sent: Sentence) extends TermExpression
  case class WithoutSentence(termExp: TermExpression, postquals: List[Postqualification])
        extends TermExpression
  case class NumTerm(num: Numeral) extends TermExpression
  case class TermAndTypeExp(termExp: TermExpression, typExp: TypeExpression)
        extends TermExpression
  case class SelectorSymExp(selectSym: Symbol, termExp: TermExpression)
        extends TermExpression
  case class SelectorSymbol(sym: Symbol) extends TermExpression
  case object It extends TermExpression


  sealed trait Arguments
  case class TermExpressionList(termExps: List[TermExpression]) extends Arguments

  case class AdjectiveArguments(termExps: TermExpressionList)

  case class Postqualification(postSegs: List[PostqualiSegment])
  case class PostqualiSegment(postqualVars: List[Identifier],
        optTypeExp: Option[TypeExpression])

  sealed trait PrivateDefinitionParameter extends TermExpression
  case object Para1 extends PrivateDefinitionParameter
  case object Para2 extends PrivateDefinitionParameter
  case object Para3 extends PrivateDefinitionParameter
  case object Para4 extends PrivateDefinitionParameter
  case object Para5 extends PrivateDefinitionParameter
  case object Para6 extends PrivateDefinitionParameter
  case object Para7 extends PrivateDefinitionParameter
  case object Para8 extends PrivateDefinitionParameter
  case object Para9 extends PrivateDefinitionParameter
  case object Para10 extends PrivateDefinitionParameter

}
