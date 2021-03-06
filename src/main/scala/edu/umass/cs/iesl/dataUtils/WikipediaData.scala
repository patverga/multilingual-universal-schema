package edu.umass.cs.iesl.dataUtils

import java.io.File

import edu.umass.cs.iesl.entity_embeddings.data_structures.{DocLanguage, ELDocument}
import edu.umass.cs.iesl.entity_embeddings.embedding.EntityResolver
import edu.umass.cs.iesl.entity_embeddings.load.LoadWikipediaArticlesCrossWikis
import edu.umass.cs.iesl.process._

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
  val linker = EmbeddingLinkedRawTextProcesser.initializeLinker(opts)
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
        EntityLinkedLogPatternsRelations.process(fDoc)
        val result = EmbeddingLinkedRawTextProcesser.formatRelationsForExport(fDoc, opts.format.value == "arvind")
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
  val filesPerDir = 256
  val opts = new MultilingualUniversalSchemaOpts
  opts.parse(args)
  val lang = DocLanguage.fromIsoString(opts.language.value)
  val entityResolver = EntityResolver.fromCMDOptions(opts, lang)
  val wikiProcessor = new LoadWikipediaArticlesCrossWikis(lang, entityResolver)
  val wikiIterator = wikiProcessor.fromCompressedFilename(opts.inputFileName.value)

  println(s"Exporting wiki data at ${opts.inputFileName.value}")
  var outerDir = "0"
  var innerDir = "0"
  var dir = s"wikis/${opts.language.value}/$outerDir/"
  var i = 0
  while (wikiIterator.hasNext){
    val wikiArticle = wikiIterator.next()
    // skip special pages
    if (!wikiArticle.isSpecialPage && !wikiArticle.isCategoryPage
      && !wikiArticle.isRedirect && !wikiArticle.isDisambiguationPage
      && !wikiArticle.isListOfPage && !wikiArticle.isInternalPage){
      val outFile = s"${wikiArticle.title.replaceAll(" ","_")}.bz"
      if (i % filesPerDir == 0) {
        val tmp = innerDir.toInt + 1
        if (tmp > filesPerDir) {
          outerDir = (outerDir.toInt + 1).toString
          innerDir = "0"
        } else innerDir = tmp.toString
        dir = s"wikis/${opts.language.value}/$outerDir/$innerDir"
        val outDirFile = new File(dir)
        if (!outDirFile.exists()) outDirFile.mkdirs()
        println(i, outFile)
      }
      try {
        IO.exportStringToFile(s"$dir/$outFile", wikiArticle.rawDocumentText, append = false, compress = true)
      } catch {case e : Exception => println(s"Could not export ${wikiArticle.title} to file $dir/$outFile")}
      i+= 1
    }
  }
}
