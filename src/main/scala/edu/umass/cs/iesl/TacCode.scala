package edu.umass.cs.iesl

import cc.factorie.app.nlp.{Token, Document, DocumentAnnotator}
import cc.factorie.app.nlp.coref.{ConllProperNounPhraseFinder}

/**
 * Created by pv on 3/6/15.
 */

trait NLPComponent extends DocumentAnnotator {
  // "doc" only contains raw text.
  // need to fill the doc with the text, tokenized,
  // sentence segmented, pos and NER-ed
  // and return it
  def prereqAttrs: Iterable[Class[_]] = null
  def postAttrs: Iterable[Class[_]] = null
  def process1(doc: Document): Document
  override def process(doc: Document): Document = { throw new IllegalStateException("In order to make everything minimally compatible with DocumentAnnotators, you should be calling process1()") }
  // load the nlp component models
  def init(): Unit = ()
  def tokenAnnotationString(t: Token): String = ""
}

trait ChainedNLPComponent extends NLPComponent {
  val components: Seq[NLPComponent]
  private lazy val processPipeline: (Document) => Document =
    components.map(c => (d: Document) => c.process1(d)).reduceLeft(_ andThen _)
  def process1(doc: Document): Document = processPipeline(doc)
  private def initPipeline() = components.foreach(c => c.init())
  override def init(): Unit = initPipeline()
}

object DeterministicSubstringNerCorefComponent extends NLPComponent {
  //  lazy val nerCoref = TacDeterministicSubstringCoref
  lazy val nerCoref = TacDeterministicSubstringCoref
  def process1(doc: Document): Document = {
    print(s"\tDeterministicSubstringCoref[${doc.name}]\n")
    nerCoref.process(doc)
    doc
  }
}

object SlotFillingLogPatternRelationMentionFindingComponent extends NLPComponent {
  def process1(doc: Document): Document = {
    println(s"\tSlotFillingRelationMentionFinding[${doc.name}]")
    SlotFillingLogPatternsRelationMentions.process(doc)
  }
}

object TacDeterministicSubstringCoref extends DeterministicSubstringCoref(ConllProperNounPhraseFinder)//TacNERPhraseFinder)
object SlotFillingLogPatternsRelationMentions extends LogPatternsRelationMentions(EntityTypePatterns.SLOTFILLING)

object EntityTypePatterns {
  val SLOTFILLING = "(ORG|PER|ORGANIZATION|PERSON)"
  val COLDSTART = "(ORG|PER|GPE|ORGANIZATION|PERSON|GPE:COUNTRY|GPE:STATE_PROVINCE|GPE:CITY)"
  val EVENT ="(LOC|WEA|SENTENCE|GPE|NUMERIC|CRIME|CONTACT_INFO|PER|ORG|DATE|FAC|JOB_TITLE|VEH)"
}

