/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package edu.umass.cs.iesl

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.coref.{WithinDocCoref, MentionPhraseFinder, ConllProperNounPhraseFinder}
import cc.factorie.app.nlp.phrase.Phrase
import cc.factorie.app.nlp.pos.PennPosTag

/** A dead-simple deterministic coreference system that operates only on named entities
    and resolves coreference only by exact string suffix match. That is, the canonical mention
    is the longest mention, and its coreferents are suffixes of that string. */
object DeterministicSubstringCoref extends DeterministicSubstringCoref(ConllProperNounPhraseFinder)

class DeterministicSubstringCoref(  phraseFinder:MentionPhraseFinder) extends DocumentAnnotator {
  def prereqAttrs: Seq[Class[_]] = phraseFinder.prereqAttrs ++ Seq(classOf[PennPosTag])
  def postAttrs = Seq(classOf[WithinDocCoref])
  def tokenAnnotationString(token: Token): String = {
    val entities = token.document.coref.entities.toSeq
    token.document.coref.mentions.find(m => m.phrase.contains(token)) match {
      case Some(mention) =>
        val mtokens = mention.phrase.tokens
        if (mtokens.length == 1) "(" + entities.indexOf(mention.entity) + ")"
        else if (mtokens.indexOf(token) == 0) "(" + entities.indexOf(mention.entity)
        else if (mtokens.indexOf(token) == mtokens.length) entities.indexOf(mention.entity) + ")"
        else "_"
      case None => "_"
    }
  }
  def process(document: Document) = {
//    val phrases = phraseFinder(document)
    val phrases = document.attr[Seq[Phrase]]
    val coref = new WithinDocCoref(document)
    val phrasesSortedDecreasingByLength = phrases.toList.sortBy(_.tokensString(" ").length * -1)

    println("phrases: " + phrases.size + "\t\t phrasesSorted :" + phrasesSortedDecreasingByLength.length)
    for (phrase <- phrasesSortedDecreasingByLength) {
      val targetString = phrase.tokensString(" ").toLowerCase
      val targetIsPerson = phrase.headToken.nerTag.baseCategoryValue.startsWith("PER")
      // Find an entity whose last token string (case insensitive) matches this string
      // iff the entity is a person and this is only one token
      val entityOption = coref.entities.find(e => {
        val entityString = e.canonicalMention.string.toLowerCase
        if(targetIsPerson && phrase.length == 1) e.canonicalMention.phrase.last.string.toLowerCase == targetString
        else entityString == targetString
      })
      if (entityOption.isDefined){
        //        println(s"(1) Adding mention [${phrase.tokensString(" ")}] to entity [${entityOption.get.canonicalMention.string}]")
        coref.addMention(phrase, entityOption.get)
      } // targetString is a substring of an existing mention
      else {
        val entity = coref.newEntity
        //        println(s"(3) Adding entity [${phrase.tokensString(" ")}]")
        val mention = coref.addMention(phrase, entity)
        entity.canonicalMention = mention
      }
    }
    document.attr += coref
    if (!document.annotators.contains(classOf[WithinDocCoref]))
      document.annotators(classOf[WithinDocCoref]) = this.getClass
    document
  }
}
