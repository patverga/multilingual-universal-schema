package edu.umass.cs.iesl

import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.ner.ConllChainNer
import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger
import edu.umass.cs.iesl.entity_embeddings.EntityEmbeddingOpts
import edu.umass.cs.iesl.entity_embeddings.data_structures.{ELDocument, English}

/**
 * Created by pv on 3/5/15.
 */


object RelationComponents extends ChainedNLPComponent {
  lazy val components = Seq(
    DeterministicSubstringNerCorefComponent,
    SlotFillingLogPatternRelationMentionFindingComponent
  )
}

object UniversalSchemaPipeline extends App
{
  val opts = new EntityEmbeddingOpts
  opts.parse(args)
  // Document text
  val exampleDocumentText = "The last time I went to Boston, I visited the home of Paul Revere in Quincy. I also visited the MFA and ate lunch with my friend at Harvard."
  val entityLinker = new EmbeddingEntiyLinking(opts)
  val exampleDoc = entityLinker.linkText(exampleDocumentText)
  RelationComponents.process1(exampleDoc)
  println("didnt crash")
}

object TestRelationComponents extends App{
  // Document text
  val exampleDocumentText = "The last time I went to Boston, I visited the home of Paul Revere in Quincy. I also visited the MFA and ate lunch with my friend at Harvard."
  // Document Representation for Entity linking
  val elDoc = ELDocument("test", exampleDocumentText,lang=English)
  // Convert to a factorie document
  val fDoc = elDoc.toFactorieDocument
  // Define the mention finder
  OntonotesForwardPosTagger.process(fDoc)
  ConllChainNer.process(fDoc)
  EnglishNERMentionFinder.process(fDoc)
  RelationComponents.process1(fDoc)
  println("didnt crash")
}
