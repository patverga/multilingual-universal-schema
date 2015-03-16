package edu.umass.cs.iesl

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.ner.NerTag
import cc.factorie.app.nlp.pos.PosTag
import cc.factorie.app.nlp.relation.{TACRelation, TACRelationList}
import cc.factorie.util.Attr
import cc.factorie.variable.ArrowVariable
import edu.umass.cs.iesl.entity_embeddings.data_structures.{EntityLinks, EntityRef, UnsluggedDocument}

import scala.collection.mutable.ArrayBuffer

object LogPatternsRelations extends LogPatternsRelations("(ORG|PER|LOC|ORGANIZATION|PERSON|LOCATION)")

class LogPatternsRelations(entityTypePatternString: String) extends DocumentAnnotator {
  val perOrgPattern = entityTypePatternString.r.pattern

  def process(document: Document): Document = {
    //    val coref = document.attr[WithinDocCoref]
    //    val relationMentions = new RelationMentionList
    //    val mentions = coref.mentions.sortBy(_.phrase.asInstanceOf[TokenSpan]).toList

    val elMentions = document.attr[EntityLinks]
    val relationMentions = new EntityLinkedRelationMentionList
    val mentions = elMentions.mentions.toSeq.toList
    val unsluggedDoc = document.attr[UnsluggedDocument].doc
    /** this produces a sliding window of 4 mentions that we then compare to generate contexts. Each mention should be compared
      * to the three mentions before and after it in the following loop. The last element is a singleton list which we drop.
      * The last mention in the document has already been compared to the three mentions that preceed it.
      * */
    val mentionGrouping = (0 until mentions.size).map(idx => mentions.slice(idx, math.min(idx + 4, mentions.size))).dropRight(1)
    println("mention groupings: " + mentionGrouping.length + "\t\t mentions: " + mentions.size)
    for (m1 :: ms <- mentionGrouping;
         m2 <- ms;
         //         e1 = m1.phrase;
         //         e2 = m2.phrase;
         //         e1Type = e1.headToken.nerTag.baseCategoryValue;
         //         e2Type = e2.headToken.nerTag.baseCategoryValue;
         //         e1Start = e1.tokens(0).positionInSentence;
         //         e1End = e1.tokens.last.positionInSentence + 1;
         //         e2Start = e2.tokens(0).positionInSentence;
         //         e2End = e2.tokens.last.positionInSentence + 1;
         //         toks = e1.sentence.tokens.map(_.string).toArray;
         //         if e1.sentence == e2.sentence) {
         e1 = m1.span;
         e2 = m2.span;
         e1Unslugged = unsluggedDoc.asSection(e1.head.positionInSection);
         e2Unslugged = unsluggedDoc.asSection(e2.head.positionInSection);
         e1Type = e1Unslugged.nerTag.baseCategoryValue;
         e2Type = e2Unslugged.nerTag.baseCategoryValue;
         e1Start = e1Unslugged.positionInSentence;
         e1End = e1Start + e1.length;
         e2Start = e2Unslugged.positionInSentence;
         e2End = e2Start + e2.length;
         toks = e1Unslugged.sentence.tokens.map(_.string).toArray;
         if e1Unslugged.sentence == e2Unslugged.sentence) {
      // Entity1 is person or organization: add arg1 arg2 pattern
//      if (perOrgPattern.matcher(e1Type).matches) {
        val pat = patternLog(toks, e1Start, e1End, e2Start, e2End)
        //relationMentions += new RelationMention(m1, m2, "surface", pat)

      val m = if (e1End <= e2Start) new EntityLinkedRelationMention(m1, m2, true) else new EntityLinkedRelationMention(m2, m1, true)
        m._relations += new TACRelation(pat, 1.0, pat)
        relationMentions += m
//      }
//      // Entity2 is person or organization: add arg2 arg1 pattern
//      if (perOrgPattern.matcher(e2Type).matches) {
//        val pat = patternLog(toks, e2Start, e2End, e1Start, e1End)
//        //relationMentions += new RelationMention(m2, m1, "surface", pat)
//        val m = new EntityLinkedRelationMention(m1, m2, false) //, "surface", pat)
//        m._relations += new TACRelation(pat, 1.0, pat)
//        relationMentions += m
//      }
    }

    document.attr += relationMentions
    document
  }

  // should also have Mention
  //  def prereqAttrs: Iterable[Class[_]] = List(classOf[Sentence], classOf[PennPosTag], classOf[NerTag], classOf[WithinDocCoref])
  //  def postAttrs: Iterable[Class[_]] = List(classOf[RelationMentionList])

  def prereqAttrs: Iterable[Class[_]] = List(classOf[Sentence], classOf[PosTag], classOf[NerTag], classOf[EntityLinks])
  def postAttrs: Iterable[Class[_]] = List(classOf[EntityLinkedRelationMentionList])

  def tokenAnnotationString(token: Token) = ???

  def patternLog(toks: Array[String], qStart: Int, qEnd: Int, slStart: Int, slEnd: Int): String = {
    val isQueryFirst = qEnd <= slStart;
    val patStart = if (isQueryFirst) qEnd else slEnd
    val patEnd = if (isQueryFirst) slStart else qStart
    val firstArg = "$ARG1" //if (isQueryFirst) "$ARG1" else "$ARG2"
    val secondArg = "$ARG2" //if (isQueryFirst) "$ARG2" else "$ARG1"
    val pat =
      if (patStart == patEnd) {
        firstArg + " " + secondArg
      } else if (patEnd - patStart <= 4) {
        firstArg + " " + toks.slice(patStart, patEnd).mkString(" ") + " " + secondArg
      } else {
        val logBin = (scala.math.log(patEnd - patStart - 4) / scala.math.log(2)).toInt.toString
        firstArg + " " + toks.slice(patStart, patStart + 2).mkString(" ") + " [" + logBin + "] " +
          toks.slice(patEnd - 2, patEnd).mkString(" ") + " " + secondArg
      }
    pat
  }
}

class EntityLinkedRelationMention(val arg1: EntityRef, val arg2: EntityRef, var isArg1First: Boolean = true) extends ArrowVariable(arg1, arg2) with Attr {
  val _relations = ArrayBuffer[TACRelation]()
  this.attr += TACRelationList(_relations)

  def relations = this.attr[TACRelationList]
}

class EntityLinkedRelationMentionList extends ArrayBuffer[EntityLinkedRelationMention]() with Attr
