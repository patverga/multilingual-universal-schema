package edu.umass.cs.iesl.process

import java.io.File

import cc.factorie.app.nlp.load.TACDocTypes.TACDocumentType
import cc.factorie.app.nlp.{CompoundDocumentAnnotator, Document}
import cc.factorie.app.nlp.load.{TacFileIterator, TACSectionalizer}
import cc.factorie.app.nlp.segment.DeterministicNormalizingHtmlTokenizer
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

/**
 * Process raw text using Embeddings Entity Linker
 */
object EmbeddingLinkedRawTextProcesser extends ProcessDataForUniversalSchema
{
  def main(args : Array[String]) {
    val opts = new MultilingualUniversalSchemaOpts
    opts.parse(args)

    val linker = initializeLinker(opts)
    val mentionFinder = if (opts.language.value == "es") SpanishNERMentionFinder else EnglishNERMentionFinder

    batchProcess(opts, mentionFinder, linker)
  }

  def initializeLinker(opts : MultilingualUniversalSchemaOpts): LogisticRegressionTrainedLinker =
  {
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
}

/**
 * Process raw text using exact string match linking to freebase
 */
object ExactStringLinkedRawTextProcesser extends ProcessDataForUniversalSchema
{
  def main(args : Array[String]) {
    val opts = new MultilingualUniversalSchemaOpts
    opts.parse(args)

    val mentionFinder = if (opts.language.value == "es") SpanishNERMentionFinder else EnglishNERMentionFinder
    batchProcess(opts, mentionFinder, ExactStringFreebaseLink)
  }
}

/**
 * Process TAC formated SGML docs using exact string match linking to freebase
 */
object ExactStringLinkedTACProcessor extends ProcessDataForUniversalSchema
{

  val annos = Seq(
    DeterministicNormalizingHtmlTokenizer,   // tokenize, normalize
    TACSectionalizer                       // remove sgml
  )
  val compound = new CompoundDocumentAnnotator(annos)

  def main(args:Array[String]) {
    val opts = new MultilingualUniversalSchemaOpts
    opts parse args

    val lang = DocLanguage.fromIsoString(opts.language.value)

    val tacFiles = new TacFileIterator(new File(opts.inputFileName.value))

    println("Processing files...")
    val elDocs = tacFiles.map ( serDoc =>
    {
      val doc = new Document(serDoc.docString).setName(serDoc.id)
      doc.attr += TACDocumentType.fromFilePath(new File("newswire"))
      doc.annotators += classOf[TACDocumentType] -> classOf[TACDocumentType]

      compound.process(doc)
      ELDocument(serDoc.id, doc.sections(1).string, lang = lang)
    }).toSeq

    println("Mention Finding and Entity Linking...")
    val mentionFinder = if (lang == Spanish) SpanishNERMentionFinder else EnglishNERMentionFinder
    batchProcess(opts, mentionFinder, ExactStringFreebaseLink, elDocs)
  }
}


/**
 * super class handling the processing and exporting of data
 */
abstract class ProcessDataForUniversalSchema
{
  val batchSize = 256

  def batchProcess(opts: MultilingualUniversalSchemaOpts, mentionFinder: MultilingualNERMentionFinder,
                   entityLinker : EntityLinker): Unit = {
    batchProcess(opts, mentionFinder, entityLinker, IO.loadPlainTextTestData(opts))
  }

  def batchProcess(opts: MultilingualUniversalSchemaOpts, mentionFinder: MultilingualNERMentionFinder,
                   entityLinker : EntityLinker, elDocs: Seq[ELDocument]): Unit =
  {
    val arvindFormat = opts.format.value == "arvind"
    var i = 0
    while (i < elDocs.size) {
      // hack to deal with ner lexicon oading not being threadsafe bug
      val batch = if (i == 0) Seq(elDocs.head) else elDocs.slice(i, Math.min(i + batchSize, elDocs.size))
      val result = processELDocs(batch, mentionFinder, entityLinker, opts.threads.value.toInt > 1, arvindFormat = arvindFormat)
      println(result)
      if (opts.outputFileName.wasInvoked) {
        IO.exportStringToFile(opts.outputFileName.value, result, append = true)
      }
      i += batch.size
    }
  }

  def processELDocs(elDocs : Seq[ELDocument], mentionFinder: MultilingualNERMentionFinder,
                    linker : EntityLinker, parallel : Boolean = true, arvindFormat : Boolean = false): String = {
    // load data and process each doc in parallel
    val docs = if (parallel) elDocs.par else elDocs
    docs.zipWithIndex.map { case (elDoc, i) =>
      //      println(s"Processing document $i")
      // Convert to a factorie document
      val fDoc = elDoc.toFactorieDocument

      mentionFinder.process(fDoc)
      linker.process(fDoc)
      EntityLinkedLogPatternsRelations.process(fDoc)

      formatRelationsForExport(fDoc, arvindFormat)
    }.mkString("")
  }

  def formatRelationsForExport(doc: Document, arvindFormat : Boolean = false): String = {
    val sb = new StringBuilder
    val entitySeparator = if (arvindFormat) "," else "\t"
    val relationMentionList = doc.attr[EntityLinkedRelationMentionList]
    relationMentionList.foreach(rm => {
      rm._relations.foreach(r =>
      {
        val e1 = Slug.unSlug(rm.arg1.entitySlug)
        val e2 = Slug.unSlug(rm.arg2.entitySlug)
        println(e1, e2)
        val fbid1 = FreebaseWikiBiMap(WikipediaId(e1))
        val fbid2 = FreebaseWikiBiMap(WikipediaId(e2))
        val formatedRelation = if(arvindFormat) r.provenance.replaceAll(" ",",") else r.provenance

        if (fbid1 != None && fbid2 != None) {
//          sb.append(s"$e1$entitySeparator")
          sb.append(s"${fbid1.get.value}$entitySeparator") // id1 nertag
//          sb.append(s"$e2$entitySeparator")
          sb.append(s"${fbid2.get.value}\t") // id2 nertag
          sb.append(s"$formatedRelation\t") // evidence
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


class MultilingualUniversalSchemaOpts extends EntityEmbeddingOpts{
  val inputFileName = new CmdOption[String]("input-filename", "", "FILENAME", "The filename of the raw text input data.")
  val inputDirName = new CmdOption[String]("input-directory", "", "DIRNAME", "A directory containing different text files.")
  val outputFileName = new CmdOption[String]("output-filename", "", "FILENAME", "File to output results to.")
  val threads = new CmdOption[String]("threads", "24", "INT", "Number of threads to use.")
  val format = new CmdOption[String]("format", "tac", "STRING", "Format as factorie tac format or arvind format.")
}