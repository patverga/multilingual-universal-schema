package edu.umass.cs.iesl

import cc.factorie.app.nlp.Document
import cc.factorie.la.DenseTensor1
import edu.umass.cs.iesl.entity_embeddings.EntityEmbeddingOpts
import edu.umass.cs.iesl.entity_embeddings.data_structures._
import edu.umass.cs.iesl.entity_embeddings.data_structures.data_stores.{EmbeddingCollection, SurfaceFormDB, TypeDB}
import edu.umass.cs.iesl.entity_embeddings.linking.LogisticRegressionTrainedLinker
import edu.umass.cs.iesl.entity_embeddings.util.FileIO

/**
 * Created by pv on 3/5/15.
 */


object UniversalSchemaPipeline extends App
{
  // Document text
  val exampleDocumentText = "The last time I went to Boston, I visited the home of Paul Revere in Quincy. I also visited the MFA and ate lunch with my friend at Harvard."
  // Document Representation for Entity linking
  val elDoc = ELDocument("test", exampleDocumentText,lang=English)
  // Convert to a factorie document
  val fDoc = elDoc.toFactorieDocument

  EnglishNERMentionFinder.process(fDoc)
  val linker = initializeLinker(args)
  linker.process(fDoc)
  SlotFillingLogPatternRelationMentionFindingComponent.process1(fDoc)


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
    val relationMentionList = doc.attr[RelationMentionList2]
    relationMentionList.foreach(rm => {
      rm._relations.foreach(r => {
        sb.append(s"${Slug.unSlug(rm.arg1.entitySlug)}\t") //${rm.arg1.span.head.nerTag.baseCategoryValue}\t") // id1 nertag
        sb.append(s"${Slug.unSlug(rm.arg2.entitySlug)}\t") //${rm.arg2.span.head.nerTag.baseCategoryValue}\t") // id2 nertag
        sb.append(s"${Slug.unSlug(rm.arg1.span.string)}\t${Slug.unSlug(rm.arg2.span.string)}\t") // string1 string2
        sb.append(s"${doc.name}\t") // docid
        sb.append(s"${rm.arg1.span.head.stringStart}-${rm.arg1.span.last.stringEnd}:") // first mention offsets
        sb.append(s"${rm.arg2.span.head.stringStart}-${rm.arg2.span.last.stringEnd}:") // second mention offsets
//        sb.append(s"${rm.arg1.span.sentence.head.stringStart}-${rm.arg2.span.sentence.last.stringEnd}\t") // whole sentence offsets
        sb.append(s"${r.provenance}") // evidence
        sb.append("\n")
      })
    })
    sb.toString()
  }

}

object RelationComponents extends ChainedNLPComponent {
  lazy val components = Seq(
    DeterministicSubstringNerCorefComponent,
    SlotFillingLogPatternRelationMentionFindingComponent
  )
}
