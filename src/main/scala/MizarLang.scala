//package mizar

import org.parboiled2._

object MizarLang {

  case class Identifier(name: String)
  case class FileName(name: String)
  case class Symbol(name: String)
  case class Numerals(name: String)


  case class Article(environ: EnvironmentDeclaration, text: TextProper)


////////////////////////        Environment         ////////////////////////
  type EnvironmentDeclaration = List[Directive]

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

  type Section = List[TextItem]

  sealed trait TextItem
  case class Reservation(segments: List[ReservationSegment]) extends TextItem
  case class DefinitionalItem(defList: List[DefinitionalBlock]) extends TextItem
  case class RegistrationItem(regisList: List[RegistrationBlock]) extends TextItem
  case class NotationItem(noteList: List[NotationBlock]) extends TextItem
  case class Theorem(thm: CompactStatement) extends TextItem
  case class SchemeItem(schemeList: List[SchemeBlock]) extends TextItem


//// 1st layer expansion
  case class ReservationSegment(reservedIdentifiers: List[Identifier],
                                typ : TypeExpression)


  sealed trait DefinitionalBlock
  sealed trait DefinitionItem extends DefinitionalBlock
  sealed trait Definition extends DefinitionalBlock
  sealed trait Redefinition extends DefinitionalBlock


  sealed trait RegistrationBlock
  sealed trait ClusterRegistration extends RegistrationBlock ////// subclass not declared
  case class IdentifyRegistration(identifyPatt: FunctorPattern,
        withPatt: FunctorPattern, locusState: List[LocusStatement],
        corrCond: CorrectConditions) extends RegistrationBlock
  case class PropertyRegistration(typ: TypeExpression, just: Justification)
        extends RegistrationBlock
  case class ReductionRegistration(reduceTerm: TermExpression,
        toTerm: TermExpression, corrCond: CorrectConditions)
        extends RegistrationBlock
  sealed trait AuxiliaryItem extends RegistrationBlock with DefinitionItem //// subclass not declared


  sealed trait NotationBlock
  case class LociDeclaration(qualVar: QualifiedVariable,
        conds: List[Conditions]) extends RegistrationBlock with NotationBlock
        with DefinitionItem
  sealed trait NotationDeclaration extends NotationBlock

  case class SchemeBlock(schemeId: Identifier, schParameters: List[SchemeSegment],
    schConcl: Sentence, schPremise: List[List[Proposition]], reason: Reasoning)



//// 2nd layer expansion
  case class PermissiveAssumption(assump: Assumption) extends DefinitionItem


  case class StructureDefinition(ancestors: List[StructureTypeExpression],
        structsymbol: Symbol, locusList: List[Locus],
        fieldSegs: List[FieldSegment] ) extends Definition
  case class ModeDefinition(modePatt: ModePattern, modeBlock: ModeBlock,
        modeProp: List[ModeProperty] ) extends Definition with Redefinition
  case class FunctorDefinition(funPatt: FunctorPattern,
        specs: List[Specification], defiens: List[Definiens],
        corrCond: CorrectConditions, funcProp: List[FunctorProperty])
        extends Definition with Redefinition
  case class PredicateDefinition(predPatt: PredicatePattern,
        defiens: List[Definiens], corrCond: CorrectConditions,
        predProp: List[PredicateProperty]) extends Definition with Redefinition
  case class AttributeDefinition(attrPatt: AttributePattern, defiens: Definiens,
        corrCond: CorrectConditions) extends Definition with Redefinition


  case class ExistentialRegistration(adjList: List[Adjective],
        typ: TypeExpression, corrCond: CorrectConditions)
        extends ClusterRegistration
  case class ConditionalRegistration(adjList: List[Adjective],
        toAdjList: List[Adjective], typ: TypeExpression, corrCond: CorrectConditions)
        extends ClusterRegistration
  case class FunctorialRegistration(termExp: TermExpression, toAdjList: List[Adjective],
        typ: TypeExpression, corrCond: CorrectConditions)
        extends ClusterRegistration


  sealed trait Statement extends AuxiliaryItem
  sealed trait PrivateDefinition extends AuxiliaryItem


  case class AtrributeSynonym(patt: AttributePattern, forPatt: AttributePattern)
        extends NotationDeclaration
  case class AtrributeAntonym(patt: AttributePattern, forPatt: AttributePattern)
        extends NotationDeclaration
  case class FunctorSynonym(patt: FunctorPattern, forPatt: FunctorPattern)
        extends NotationDeclaration
  case class ModeSynonym(patt: ModePattern, forPatt: ModePattern)
        extends NotationDeclaration
  case class PredicateSynonym(patt: PredicatePattern, forPatt: PredicatePattern)
        extends NotationDeclaration
  case class PredicateAntonym(patt: PredicatePattern, forPatt: PredicatePattern)
        extends NotationDeclaration


  case class Proposition(label: List[Identifier], sent: Sentence)
  type Sentence = FormulaExpression
  sealed trait Justification
  sealed trait SimpleJustification extends Justification
  case class Proof(pf: Reasoning) extends Justification


  sealed trait SchemeSegment
  case class PredicateSegment(predId: List[Identifier],
        typList: List[List[TypeExpression]]) extends SchemeSegment
  case class FunctorSegment(funcId: List[Identifier],
        typList: List[List[TypeExpression]], spec: Specification)
        extends SchemeSegment

  case class Reasoning(reasItems: List[ReasoningItems],
        casesOrSupposeList: List[CasesOrSuppose])
  sealed trait CasesOrSuppose{
    val just : SimpleJustification
  }
  case class CaseList(just: SimpleJustification, cases: List[Case])
        extends CasesOrSuppose
  case class SupposeList(just: SimpleJustification, supps: List[Suppose])
        extends CasesOrSuppose


///// 3rd layer expansion
  sealed trait Assumption
  case class SingleAssump(prop: Proposition) extends Assumption
  case class CollectiveAssump(conds: Conditions) extends Assumption
  case class ExistentialAssump(qualVar: QualifiedVariable, optCond: List[Conditions])
        extends Assumption

  case class Locus(varIden: VariableIdentifier)
  type VariableIdentifier = Identifier

  case class FieldSegment(selectorSym: List[Symbol], spec: Specification)

  case class ModePattern(modeSym: ModeSymbol, optloci: List[List[Locus]])

  case class ModeBlock(specs: List[Specification], defiens: List[Definiens],
        subblock: ModeSubBlock)
  sealed trait ModeSubBlock
  case class CorrectConditions(condList: List[CorrectCondition],
        optjust: List[Justification]) extends ModeSubBlock

  type ModeProperty = Justification

  sealed trait FunctorPattern
  case class FuncSymbolLoci(opt1: List[FunctorLoci], funcSym: FunctorSymbol,
        opt2: List[FunctorLoci]) extends FunctorPattern
  case class FuncBracketLoci(lBracket: LeftFunctorBracket, loci: List[Locus],
        rBracket: RightFunctorBracket) extends FunctorPattern

  type Specification = TypeExpression

  sealed trait Definiens
  case class SimpleDefiniens(optLabel: List[Identifier], sentExp: SentOrTerm)
        extends Definiens
  sealed trait SentOrTerm
  case class ConditionalDefiniens(optLabel: List[Identifier],
        partialList: List[PartialDefiniens], optSentExp: List[SentOrTerm])
        extends Definiens

  type CorrectCondition = Justification

  type FunctorProperty = Justification

  case class PredicatePattern(opt1loci: List[List[Locus]], predSym: PredicateSymbol,
        opt2loci: List[List[Locus]])

  type PredicateProperty = Justification

  case class AttributePattern(locus: Locus, optloci: List[List[Locus]],
        attSym: Symbol)

  case class Adjective(optArg: List[AdjectiveArguments], attSym: Symbol)

  case class DiffuseStatement(optLabel: List[Identifier], reason: Reasoning)
        extends Statement
  sealed trait LinkableStatement extends Statement

  case class CompactStatement(prop: Proposition, just: Justification)
        extends LinkableStatement
  case class ChoiceStatement(qualVar: QualifiedVariable, conds: Conditions,
        just: SimpleJustification) extends LinkableStatement
  case class TypeChangingStatement(changeList: TypeChangeList,
        typExp: TypeExpression, just: SimpleJustification) extends LinkableStatement
  case class IterativeEquality(optLabel: List[Identifier], termExp: TermExpression,
        equalsTerm: List[TermAndJustification]) extends LinkableStatement
  case class TermAndJustification(term: TermExpression, just: SimpleJustification)


  case class ConstantDefinition(eqList: List[Equating]) extends PrivateDefinition
  case class PrivateFunctorDefinition(patt: PrivateFunctorPattern,
        term: TermExpression) extends PrivateDefinition
  case class PrivatePredicateDefinition(patt: PrivatePreicatePattern,
        sent: Sentence) extends PrivateDefinition


  case class StraightforwardJustification(optReferences: List[References])
        extends SimpleJustification
  case class SchemeJustification(schRefer: SchemeReference,
        optReferences: List[References]) extends SimpleJustification

  case class Case(propList: List[Proposition], reason: Reasoning)
  case class Suppose(propList: List[Proposition], reason: Reasoning)





///// defining for compilation. Comment out as their original defitions gets defined.
  case class TypeExpression() extends ModeSubBlock
  case class LocusStatement()
  case class TermExpression() extends SentOrTerm
  case class QualifiedVariable()
  case class TypeChangeList()
  case class PrivateFunctorPattern()
  case class PrivatePreicatePattern()
  case class References()
  case class SchemeReference()

  type Conditions = List[Proposition]

  case class PredicateSymbol()
  case class PartialDefiniens()
  case class AdjectiveArguments()
  case class StructureTypeExpression()

  case class Equating()
  case class ModeSymbol()
  case class FunctorLoci()
  case class FunctorSymbol()
  case class LeftFunctorBracket()
  case class RightFunctorBracket()
  case class ReasoningItems()

  case class FormulaExpression() extends SentOrTerm


}

/*
  sealed trait ModeBlock
  case class SpecsDefCorrCond(specs: List[Specification],
        defiens: List[Definiens], corrCond: CorrectConditions)
        extends ModeBlock



  sealed trait TermExpression



  sealed trait ModeSymbol

  case class ModeSym(sym: Symbol) extends ModeSymbol

  case object setSymbol extends ModeSymbol

  sealed trait RadixType

  case class ModeRadixType(
      mode: ModeSymbol,
      l: List[TermExpression]) extends RadixType

  case class StrucRadixType(
      structureSymbol: Symbol,
      l : List[TermExpression]) extends RadixType
*/

//   case class PrivateDefinition(param: Char) extends TermExpression


  //val p = P("a".!)
