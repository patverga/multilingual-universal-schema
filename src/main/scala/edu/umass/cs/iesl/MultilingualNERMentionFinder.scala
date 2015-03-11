package edu.umass.cs.iesl

import java.net.URL

import cc.factorie.app.nlp.coref.ConllProperNounPhraseFinder
import cc.factorie.app.nlp.ner.NoEmbeddingsConllStackedChainNer
import cc.factorie.app.nlp.pos.{SpanishChainPosTagger, OntonotesForwardPosTagger}
import cc.factorie.app.nlp.segment.{DeterministicSentenceSegmenter, DeterministicTokenizer}
import cc.factorie.app.nlp.{Document, Token, TokenSpan}
import edu.umass.cs.iesl.entity_embeddings.data_structures._
import edu.umass.cs.iesl.entity_embeddings.linking.MentionFinder


object EnglishNERMentionFinder extends MultilingualNERMentionFinder(English, false) {
  override def nerAndPosTag(unsluggedDoc: Document) {
    OntonotesForwardPosTagger.process(unsluggedDoc)
    NoEmbeddingsConllStackedChainNer.process(unsluggedDoc)
    //    OntonotesTransitionBasedParser.process(unsluggedDoc)
    //    ParseAndNerBasedPhraseFinder.process(unsluggedDoc)
  }
}

object SpanishNERMentionFinder extends MultilingualNERMentionFinder(Spanish, false) {
  override def nerAndPosTag(unsluggedDoc: Document){
    val posTagger = new SpanishChainPosTagger(new URL("./models/ChainPOS-Spanish-IULA.factorie"))
    posTagger.process(unsluggedDoc)
    val nerTagger = new NoEmbeddingsConllStackedChainNer(new URL("./models/StackedConllChainNer-SpanishConll2002.factorie"))
    nerTagger.process(unsluggedDoc)
  }
}

//class EnlishNERMentionFinder(surfaceFormDB:SurfaceFormDB, lang: DocLanguage,caseSensitiveMentions:Boolean) extends MentionFinder {
abstract class MultilingualNERMentionFinder(lang: DocLanguage,caseSensitiveMentions:Boolean = false) extends MentionFinder {

  def prereqAttrs: Seq[Class[_]] = Seq(classOf[Token], classOf[DocLanguage])

  def postAttrs = Seq(classOf[EntityLinks])

  def tokenAnnotationString(token: Token): String = {
    token.document.attr[EntityLinks].mentions.find((m) => m.span.contains(token)) match {
      case Some(mention) =>
        val mtokens = mention.span.tokens
        if (mtokens.length == 1) "(" + mention.span.toString() + ")"
        else if (mtokens.indexOf(token) == 0) "(" + mention.span.toString()
        else if (mtokens.indexOf(token) == mtokens.length - 1) mention.span.toString() + ")"
        else "_"
      case None => "_"
    }
  }

  def process(document: Document) = {
    println("EnglishNERMentionFinder")
    if (!document.attr.contains(classOf[EntityLinks])) {
      println("Adding Entity Link Structure.")
      document.attr += new EntityLinks()
    }

    val unsluggedDoc = if (document.attr.contains[UnsluggedDocument])
      document.attr[UnsluggedDocument].doc
    else {
      println("Warning couldn't find unslugged document.")
      val sluggedText = document.string
      val unsluggedText = Slug.unSlug(sluggedText)
      val res = new Document(unsluggedText)
      DeterministicTokenizer.process(res)
      DeterministicSentenceSegmenter.process(res)
    }

    nerAndPosTag(unsluggedDoc)
    val mentions = ConllProperNounPhraseFinder.apply(unsluggedDoc)
    unsluggedDoc.attr += mentions
    
    val links = document.attr[EntityLinks]
    for (mention <- mentions) {
      // Note that mention will be unslugged so we need to slug it.
      val mentionSlug = Slug.toSlug(mention.string, MentionSlug, document.attr[DocLanguage])
      // Then we just need to find the corresponding token span in the slugged document
      val ref = new EntityRef(entity_embeddings.NIL_ENTITY, mentionSlug, new TokenSpan(document.asSection, mention.start, mention.end - mention.start), true)

      if (Slug.unSlug(ref.mentionSlug).replaceAllLiterally("_", " ").trim != Slug.unSlug(ref.span.string).replaceAllLiterally("_", " ").trim) {
        System.err.println("THERE IS A SERIOUS PROBLEM")
        System.err.println("Mention Slug: " + ref.mentionSlug)
        System.err.println("Span: " + ref.span.string)
        System.err.println("DOCUMENT: " + document.name)
      }

      ref.unsluggedMentionCharOffset = mention.characterOffsets._1
      ref.unsluggedMentionCharLen = mention.characterOffsets._2 - mention.characterOffsets._1
      links += ref
    }
    document
  }
  def nerAndPosTag(unsluggedDoc: Document)
}
