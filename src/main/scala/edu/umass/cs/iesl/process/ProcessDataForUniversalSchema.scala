package edu.umass.cs.iesl.process

import cc.factorie.app.nlp.Document
import cc.factorie.la.DenseTensor1
import edu.umass.cs.iesl.dataUtils.IO
import edu.umass.cs.iesl.entity_embeddings.EntityEmbeddingOpts
import edu.umass.cs.iesl.entity_embeddings.data_structures._
import edu.umass.cs.iesl.entity_embeddings.data_structures.data_stores.{EmbeddingCollection, SurfaceFormDB, TypeDB}
import edu.umass.cs.iesl.entity_embeddings.linking.{EntityLinker, LogisticRegressionTrainedLinker}
import edu.umass.cs.iesl.entity_embeddings.util.FileIO
import edu.umass.cs.iesl.knowledgeBase.{FreebaseWikiBiMap, WikipediaId}

/**
 * Created by pv on 3/5/15.
 */


object EmbeddingEntityLinkedProcesser extends ProcessDataForUniversalSchema
{
  def main(args : Array[String]) {
    val opts = new MultilingualUniversalSchemaOpts
    opts.parse(args)

    val linker = initializeLinker(opts)
    val mentionFinder = if (opts.language.value == "es") SpanishNERMentionFinder else EnglishNERMentionFinder

    val elDocs = IO.loadPlainTextTestData(opts)
    val result = processELDocs(elDocs, mentionFinder, linker, opts.threads.value.toInt > 1)

    println(result)

    if (opts.outputFileName.wasInvoked) {
      IO.exportStringToFile(opts.outputFileName.value, result)
    }
  }

  def initializeLinker(opts : MultilingualUniversalSchemaOpts): LogisticRegressionTrainedLinker ={

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
  
  def formatRelationsForExport(doc: Document): String = {
    val sb = new StringBuilder
    val relationMentionList = doc.attr[EntityLinkedRelationMentionList]
    relationMentionList.foreach(rm => {
      rm._relations.foreach(r => {
        val e1 = Slug.unSlug(rm.arg1.entitySlug)
        val e2 = Slug.unSlug(rm.arg2.entitySlug)
        val fbid1 = FreebaseWikiBiMap(WikipediaId(e1))
        val fbid2 = FreebaseWikiBiMap(WikipediaId(e2))
        if (fbid1 != None && fbid2 != None) {
          sb.append(s"$e1\t${fbid1.get.value}\t") // id1 nertag
          sb.append(s"$e2\t${fbid2.get.value}\t") // id2 nertag
          sb.append(s"${r.provenance}\t") // evidence
          sb.append("1.0\n")
          //          sb.append("\n")
        } else {
          if (fbid1 == None) print(s"Could not link $e1.\t")
          if (fbid2 == None) print(s"Could not link $e2.")
          println()
        }
      })
    })
    sb.toString()
  }
  
}

object ExactStringFreebaseLinkedProcesser extends ProcessDataForUniversalSchema
{
  val batchSize = 256

  def main(args : Array[String]) {
    val opts = new MultilingualUniversalSchemaOpts
    opts.parse(args)

    val mentionFinder = if (opts.language.value == "es") SpanishNERMentionFinder else EnglishNERMentionFinder

    val elDocs = IO.loadPlainTextTestData(opts)
    var i = 0
    while (i < elDocs.size) {
      val batch = elDocs.slice(i, Math.min(i+batchSize, elDocs.size))
      val result = processELDocs(batch, mentionFinder, ExactStringFreebaseLink, opts.threads.value.toInt > 1)
      println(result)
      if (opts.outputFileName.wasInvoked) {
        IO.exportStringToFile(opts.outputFileName.value, result)
      }
      i += batchSize
    }
  }
  
  def formatRelationsForExport(doc: Document): String = {
    val sb = new StringBuilder
    val relationMentionList = doc.attr[EntityLinkedRelationMentionList]
    relationMentionList.foreach(rm => {
      rm._relations.foreach(r => {
        val e1 = rm.arg1.entitySlug
        val e2 = rm.arg2.entitySlug
        if (e1 != "" && e2 != "") {
          sb.append(s"$e1\t") // id1 nertag
          sb.append(s"$e2\t") // id2 nertag
          sb.append(s"${r.provenance}\t") // evidence
          sb.append("1.0\n")
          //          sb.append("\n")
        }
      })
    })
    sb.toString()
  }
  
}

abstract class ProcessDataForUniversalSchema
{

  def processELDocs(elDocs : Seq[ELDocument], mentionFinder: MultilingualNERMentionFinder,
                    linker : EntityLinker, parallel : Boolean = true): String = {
    // load data and process each doc in parallel
    val docs = if (parallel) elDocs.par else elDocs
    docs.zipWithIndex.map { case (elDoc, i) =>
      //      println(s"Processing document $i")
      // Convert to a factorie document
      val fDoc = elDoc.toFactorieDocument

      mentionFinder.process(fDoc)
      linker.process(fDoc)
      EntityLinkedLogPatternsRelations.process(fDoc)

      formatRelationsForExport(fDoc)
    }.mkString("")
  }
  def formatRelationsForExport(doc: Document): String

}


class MultilingualUniversalSchemaOpts extends EntityEmbeddingOpts{
  val inputFileName = new CmdOption[String]("input-filename", "", "FILENAME", "The filename of the raw text input data.")
  val inputDirName = new CmdOption[String]("input-directory", "", "DIRNAME", "A directory containing different text files.")
  val outputFileName = new CmdOption[String]("output-filename", "", "FILENAME", "File to output results to.")
  val threads = new CmdOption[String]("threads", "24", "INT", "Number of threads to use.")
}