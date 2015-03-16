package edu.umass.cs.iesl

import edu.umass.cs.iesl.entity_embeddings.data_structures.{Spanish, ELDocument, DocLanguage}
import edu.umass.cs.iesl.entity_embeddings.embedding.EntityResolver
import edu.umass.cs.iesl.entity_embeddings.linking.LogisticRegressionTrainedLinker
import edu.umass.cs.iesl.entity_embeddings.load.LoadWikipediaArticlesCrossWikis

import scala.collection.mutable.ArrayBuffer

/**
 * Created by pv on 3/13/15.
 */
object WikipediaProcessor extends App
{
  val batchSize = 2000
  val opts = new MultilingualUniversalSchemaOpts
  opts.parse(args)

  val lang = DocLanguage.fromIsoString(opts.language.value)
  val entityResolver = EntityResolver.fromCMDOptions(opts, lang)
  val wikiProcessor = new LoadWikipediaArticlesCrossWikis(lang, entityResolver)
  val linker = ProcessDataForUniversalSchema.initializeLinker(opts)
  val mentionFinder = if (lang == Spanish) SpanishNERMentionFinder else EnglishNERMentionFinder

  println(s"Processing wiki data at ${opts.inputFileName.value}")
  val wikiIterator = wikiProcessor.fromCompressedFilename(opts.inputFileName.value)
  var batch = new ArrayBuffer[ELDocument]
  var i = 0
  while (wikiIterator.hasNext){
    val wikiArticle = wikiIterator.next()
    batch += ELDocument(wikiArticle.title, wikiArticle.rawDocumentText, lang=lang)
    i += 1
    if (i % batchSize == 0){
      println(i, batch.size)
      // load data and process each doc in parallel
      batch.par.map (elDoc => {
        //      println(s"Processing document $i")
        // Convert to a factorie document
        val fDoc = elDoc.toFactorieDocument

        mentionFinder.process(fDoc)
        linker.process(fDoc)
        LogPatternsRelations.process(fDoc)
        val result = ProcessDataForUniversalSchema.formatRelationsForExport(fDoc)
        ProcessDataForUniversalSchema.exportRelations(s"processed_wikis/${lang}/${i}_${wikiArticle.title}", result, append = true)
        result
      })
      batch = new ArrayBuffer[ELDocument]
    }
  }
}
