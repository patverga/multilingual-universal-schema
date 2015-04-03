package edu.umass.cs.iesl.entity_embeddings.data_structures

import cc.factorie.app.nlp.segment.{DeterministicSentenceSegmenter, DeterministicTokenizer}
import cc.factorie.app.nlp.{Document, TokenSpan}
import edu.umass.cs.iesl.entity_embeddings
import edu.umass.cs.iesl.entity_embeddings.data_structures.WikificationStructs.{EmptyReferenceProblem, ReferenceProblem}
import edu.umass.cs.iesl.entity_embeddings.nlp.WhiteSpaceTokenizer


case class UnsluggedDocument(doc: Document)


case class ELDocument(title: String, rawDocumentText: String, problem:ReferenceProblem=EmptyReferenceProblem, lang:DocLanguage=English) {

  lazy val mentions = problem.mentions

  def toFactorieDocument:Document = {
    /*
    This is a hack. The problem is the mentions are annotated with character offsets in the raw text of the document.
    We need to convert every word in the document into a word slug. And so if we do that first, we lose the ability
    to snap the mentions to their tokens. And so to fix this, we first tokenize the document and snap the mentions to these
    tokens. Then we take each of these tokens and make a new token that is the word slug of the original token. We record
    the position of each mention to resolve the mentions in the slugged text.
     */
    val links = new EntityLinks
    val targetLinks = new EntityLinks
    links.target = targetLinks
    val tempDoc = new Document(rawDocumentText).setName(title)
    DeterministicTokenizer.process(tempDoc)
    // We need to segment the sentences before we slug the text because the segmenter can add periods to the end of sentences
    // which don't have periods already.
    DeterministicSentenceSegmenter.process(tempDoc)
    mentions.zipWithIndex.foreach {
      case (mention,idx) =>
        tempDoc.asSection.offsetSnapToTokens(mention.start, mention.end) match {
          case Some(ts) =>
            val ref = EntityRef(mention.entity.getOrElse(entity_embeddings.NIL_ENTITY), mention.spelling, ts, mention.linkable)
            ref.unsluggedMentionCharOffset = problem.instances(idx).offset
            ref.unsluggedMentionCharLen = problem.instances(idx).len
            ts.head.attr += ref
          case None => println("WARNING: we weren't able to align the mention %s (%d, %d) in %s".format(mention.spelling, mention.start, mention.end, title))
        }
    }


    val sluggedText: StringBuffer = new StringBuffer(rawDocumentText.length)
    // why not tokens.mkString(" ")
    tempDoc.tokens.foreach(token => {
      sluggedText.append(Slug.toSlug(token.string, WordSlug, lang)).append(" ")
    })

    /*
     * Convert to white space
     */
    val doc = new Document(sluggedText.toString).setName(title)
    WhiteSpaceTokenizer.process(doc)
    val docTokens = doc.tokens.toIndexedSeq
    var idx = 0
    for (token <- tempDoc.tokens) {
      if (token.attr.contains(classOf[EntityRef])) {
        val ref = new EntityRef(token.attr[EntityRef].entitySlug,token.attr[EntityRef].mentionSlug, new TokenSpan(docTokens.slice(idx,idx + token.attr[EntityRef].span.length)),token.attr[EntityRef].linkable)
        ref.unsluggedMentionCharOffset = token.attr[EntityRef].unsluggedMentionCharOffset
        ref.unsluggedMentionCharLen = token.attr[EntityRef].unsluggedMentionCharLen
        targetLinks += ref
      }
      idx += 1
    }

    doc.attr += UnsluggedDocument(tempDoc)
    doc.attr += links
    doc.attr += lang
    doc
  }

  def score: Double = 0
}