package edu.umass.cs.iesl

import java.io.File

import edu.umass.cs.iesl.entity_embeddings.data_structures.{ELDocument, DocLanguage}
import edu.umass.cs.iesl.entity_embeddings.embedding.EntityResolver
import edu.umass.cs.iesl.entity_embeddings.load.LoadWikipediaArticlesCrossWikis

import scala.collection.mutable.ArrayBuffer

/**
 * Created by pv on 3/18/15.
 */

/**
 * Fully process, tag, and extract contexts for wikipeida dumps
 */
object WikipediaProcessor extends App
{
  val opts = new MultilingualUniversalSchemaOpts
  opts.parse(args)
  val lang = DocLanguage.fromIsoString(opts.language.value)
  val entityResolver = EntityResolver.fromCMDOptions(opts, lang)
  val wikiProcessor = new LoadWikipediaArticlesCrossWikis(lang, entityResolver)
  val wikiIterator = wikiProcessor.fromCompressedFilename(opts.inputFileName.value)
  val mentionFinder = if (opts.language.value == "es") SpanishNERMentionFinder else EnglishNERMentionFinder

  val batchSize = 2000
  val linker = ProcessDataForUniversalSchema.initializeLinker(opts)
  println(s"Processing wiki data at ${opts.inputFileName.value}")
  var batch = new ArrayBuffer[(Int, ELDocument)]
  var i = 0
  while (wikiIterator.hasNext) {
    val wikiArticle = wikiIterator.next()
    batch += ((i, ELDocument(wikiArticle.title, wikiArticle.rawDocumentText, lang = lang)))
    i += 1
    if (i % batchSize == 0) {
      println(i, batch.size)
      // load data and process each doc in parallel
      batch.par.foreach { case (j, elDoc) =>
        //      println(s"Processing document $i")
        // Convert to a factorie document
        val fDoc = elDoc.toFactorieDocument

        mentionFinder.process(fDoc)
        linker.process(fDoc)
        LogPatternsRelations.process(fDoc)
        val result = ProcessDataForUniversalSchema.formatRelationsForExport(fDoc)
        IO.exportStringToFile(s"processed_wikis/${opts.language.value}/${j}_${elDoc.title}", result, append = true)
      }
      batch = new ArrayBuffer[(Int, ELDocument)]
    }
  }
}

/**
 * Extract each page from wikipedia dump to individual file
 */
object ExportWikis extends App
{
  val filesPerDir = 128
  val opts = new MultilingualUniversalSchemaOpts
  opts.parse(args)
  val lang = DocLanguage.fromIsoString(opts.language.value)
  val entityResolver = EntityResolver.fromCMDOptions(opts, lang)
  val wikiProcessor = new LoadWikipediaArticlesCrossWikis(lang, entityResolver)
  val wikiIterator = wikiProcessor.fromCompressedFilename(opts.inputFileName.value)

  println(s"Exporting wiki data at ${opts.inputFileName.value}")
  var dirName = "0"
  var outDir = s"wikis/${opts.language.value}/$dirName/"
  var i = 0
  while (wikiIterator.hasNext){
    val wikiArticle = wikiIterator.next()
    val outFile = s"${wikiArticle.title.replaceAll(" ","_")}.bz"
    if (i % filesPerDir == 0) {
      dirName = (dirName.toInt + 1).toString
      outDir = s"wikis/${opts.language.value}/$dirName/"
      val outDirFile = new File(outDir)
      if (!outDirFile.exists()) outDirFile.mkdirs()
      println(i, outFile)
    }
    try {
      IO.exportStringToFile(s"$outDir/$outFile", wikiArticle.rawDocumentText, append = false, compress = true)
    } catch {case e : Exception => println(s"Could not export ${wikiArticle.title} to file $outDir/$outFile")}
    i+= 1
  }
}
