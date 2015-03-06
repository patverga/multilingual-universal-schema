package edu.umass.cs.iesl

import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.ner.ConllChainNer
import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger
import cc.factorie.app.nlp.relation.RelationMentionList
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

  EnglishNERMentionFinder.process(fDoc)
  RelationComponents.process1(fDoc)
//  println("didnt crash")
  println(formatDoc(fDoc))

  def formatDoc(doc: Document): String = {
    val sb = new StringBuilder
    val relationMentionList = doc.attr[RelationMentionList]
    relationMentionList.foreach(rm => {
      rm._relations.foreach(r => {
        //        println("fb " +rm.arg1.entity.attr[FreebaseId])
        //        println("wiki " +rm.arg1.entity.attr[WikipediaId])
        //        println("sf "+rm.arg1.entity.attr[SurfaceFormId])

        sb.append(s"${rm.arg1.entity.canonicalName}\t${rm.arg1.phrase.head.nerTag.baseCategoryValue}\t") // id1 nertag
        sb.append(s"${rm.arg2.entity.canonicalName}\t${rm.arg2.phrase.head.nerTag.baseCategoryValue}\t") // id2 nertag
        sb.append(s"${rm.arg1.string}\t${rm.arg2.string}\t") // string1 string2
        sb.append(s"${doc.name}\t") // docid
        sb.append(s"${rm.arg1.phrase.head.stringStart}-${rm.arg1.phrase.last.stringEnd}:") // first mention offsets
        sb.append(s"${rm.arg2.phrase.head.stringStart}-${rm.arg2.phrase.last.stringEnd}:") // second mention offsets
        sb.append(s"${rm.arg1.phrase.sentence.head.stringStart}-${rm.arg2.phrase.sentence.last.stringEnd}\t") // whole sentence offsets
        sb.append(s"${r.provenance}") // evidence
        sb.append("\n")
      })
    })
    sb.toString()
  }
}
