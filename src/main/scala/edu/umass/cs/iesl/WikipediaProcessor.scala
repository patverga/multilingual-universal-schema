package edu.umass.cs.iesl

import edu.umass.cs.iesl.entity_embeddings.data_structures.DocLanguage
import edu.umass.cs.iesl.entity_embeddings.embedding.EntityResolver
import edu.umass.cs.iesl.entity_embeddings.load.LoadWikipediaArticlesCrossWikis

/**
 * Created by pv on 3/13/15.
 */
object WikipediaProcessor extends App
{
  val opts = new MultilingualUniversalSchemaOpts
  opts.parse(args)

  val lang = DocLanguage.fromIsoString(opts.language.value)
  val entityResolver = EntityResolver.fromCMDOptions(opts, lang)
  val wikiProcessor = new LoadWikipediaArticlesCrossWikis(lang, entityResolver)

  val wikiIterator = wikiProcessor.fromCompressedFilename(opts.inputFileName.value)
  var i = 0
  while (wikiIterator.hasNext){
    val wikiArticle = wikiIterator.next()
    println(wikiArticle.title + "\t" + i)
    i += 1
  }

}
