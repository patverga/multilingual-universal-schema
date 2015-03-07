package edu.umass.cs.iesl

import cc.factorie.app.nlp.Document
import cc.factorie.app.nlp.coref.Mention
import cc.factorie.app.nlp.relation.RelationMentionList
import cc.factorie.la.DenseTensor1
import edu.umass.cs.iesl.entity_embeddings.EntityEmbeddingOpts
import edu.umass.cs.iesl.entity_embeddings.data_structures._
import edu.umass.cs.iesl.entity_embeddings.data_structures.data_stores.{EmbeddingCollection, SurfaceFormDB, TypeDB}
import edu.umass.cs.iesl.entity_embeddings.linking.LogisticRegressionTrainedLinker
import edu.umass.cs.iesl.entity_embeddings.util.FileIO

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
  val linker = initializeLinker(args)
  linker.process(fDoc)

  println(formatDoc(fDoc))

  
  def initializeLinker(args : Array[String]): LogisticRegressionTrainedLinker ={
    val opts = new EntityEmbeddingOpts
    opts.parse(args)

    // Load the typeDB
    val typeDB = TypeDB.fromCMDOptions(opts)
    println("Loading Embeddings")
    // Load the embeddings
    val embeddingCollection = EmbeddingCollection.fromCMDOptions(opts)
    // Define the entity linker
    val features = opts.features.value.map(FeatureType.fromIsoString).toList
    val weights = new DenseTensor1(FileIO.readFeatureWeights(opts.featureWeightsFilename.value.head))
    val contextWindowSize = (opts.window.value, 10000, 10000)

    // Load the surface form database
    val surfaceFormDB = SurfaceFormDB.fromCMDOptions(opts)
    new LogisticRegressionTrainedLinker(features, embeddingCollection, surfaceFormDB,
      typeDB, null, contextWindowSize, opts.caseSensitiveWords.value, opts.caseSensitiveMentions.value, weights = weights)

  }
  
  def formatDoc(doc: Document): String = {
    val sb = new StringBuilder
    val relationMentionList = doc.attr[RelationMentionList]
    relationMentionList.foreach(rm => {
      rm._relations.foreach(r => {
        sb.append(s"${alignMentionEntityLink(rm.arg1, doc)}\t${rm.arg1.phrase.head.nerTag.baseCategoryValue}\t") // id1 nertag
        sb.append(s"${alignMentionEntityLink(rm.arg2, doc)}\t${rm.arg2.phrase.head.nerTag.baseCategoryValue}\t") // id2 nertag
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
  
  def alignMentionEntityLink(relationMention : Mention, doc : Document): String = {
    if (doc.attr[EntityLinks].mentions != null) {
      val rmHead = relationMention.phrase.head
      for (mention <- fDoc.attr[EntityLinks].mentions) {
        if (rmHead == mention.span.head)
          return mention.entitySlug
        //        println(mention.mentionSlug + " linked to " + mention.entitySlug + " URL: " + Slug.toWikiURL(mention.entitySlug))
      }
    }
    relationMention.string
  }
}
