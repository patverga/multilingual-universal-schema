package edu.umass.cs.iesl.oldtaccode

import cc.factorie.app.nlp.ner.{BilouConllNerTag, ConllNerTag, NerTag}
import cc.factorie.app.nlp.parse.ParseTree
import cc.factorie.app.nlp.pos.PennPosTag
import cc.factorie.app.nlp.{TokenString, Token, Sentence, Document}
import java.io.{File, PrintWriter}

object SerializePlainText {

  final val DEFAULT_SHORTNER = false
  final val DEFAULT_BILOU = true
  final val extension = "txt"
  final val encoding = "utf-8"

  def serializeFromFileId(doc: Document, fileId: FileId, rootDir: String, formatter: Document => String): Unit = {
    fileId.makeDirectory(rootDir)
    val outputFile = fileId.asFilepath(rootDir, extension)
    val pw = new PrintWriter(outputFile, encoding)
    pw.write(formatter(doc))
    pw.close
  }

  /**
   * Deserialize a plain-text serialized Factorie document given a FileId corresponding to the
   * serialized document.
   *
   * @see deserializeFromFile
   */
  def deserializeFromFilename(doc: Document, filename: String, nerType: String = "tac", shortNer:Boolean = DEFAULT_SHORTNER, bilou: Boolean = DEFAULT_BILOU): Document = {
    deserializeFromFile(doc, new File(filename), nerType, shortNer, bilou)
  }

  /**
   * Deserialize a plain-text serialized Factorie document given a FileId corresponding to the
   * serialized document.
   *
   * @see deserializeFromFile
   */
  def deserializeFromFileId(doc: Document, fileId: FileId, rootDir: String, nerType: String = "tac", shortNer:Boolean = DEFAULT_SHORTNER, bilou: Boolean = DEFAULT_BILOU): Document = {
    val file = fileId.asFilepath(rootDir, extension)
    deserializeFromFile(doc, file, nerType, shortNer, bilou)
  }

  /**
   * Deserialize a plain-text serialized Factorie document given a File corresponding to the
   * serialized document, this corresponds to newer serialized documents which do NOT contain
   * document start and end offsets, but DO contain parse information.
   *
   * @param doc Factorie Document to deserialize into
   * @param file plaintext file to deserialize from
   * @param shortNer whether short or long ner tags (with/without BILOU) were serialized
   * @param bilou whether ner tags use BILOU or BIO
   * @return Factorie Document populated with serialized annotations
   */
  def deserializeFromFile(doc: Document, file: File, nerType: String = "tac", shortNer:Boolean = DEFAULT_SHORTNER, bilou: Boolean = DEFAULT_BILOU): Document = {
    //    doc.clearSections
    var currentSentence: Sentence = null
    var lastTok: Token = null
    var lastTokNerBilou = "O"
    var lastTokNerCategory = "O"
    val src = io.Source.fromFile(file, encoding)
    var depInfoSeq = collection.mutable.ArrayBuffer[(Int,Int,String)]()
    src.getLines.foreach(line => {
      if (line != "") {
        //        println((line == null) + "  " + line)
        //        val Array(tokString, nerTagString, positionInSentence, startDocOffset, endDocOffset, posTagString, parseHead, parseLabel) = line.split("\t")
        val Array(tokString, nerTagString, positionInSection, positionInSentence, startDocOffset, endDocOffset, posTagString) = line.split("\t")
        if (positionInSentence.toInt == 0){
          if(!depInfoSeq.isEmpty) addDepInfo(currentSentence, depInfoSeq)
          currentSentence = new Sentence(doc)
          depInfoSeq = new collection.mutable.ArrayBuffer[(Int,Int,String)]
        }

        //        depInfoSeq.append((positionInSentence.toInt, parseHead.toInt, parseLabel))

        // Approximately re-create BI(L)O(U) value if short ner tag (sans BILOU) was serialized
        val nerTag = {
          if(shortNer){
            val tokNerBilou = {
              if (nerTagString != "O") {
                if (lastTokNerCategory == nerTagString) {
                  lastTokNerBilou match {
                    case "O" => "B"
                    case "B" => "I"
                    case "I" => "I"
                    case _ => throw new Error("Something went wrong deserializing ner tags")
                  }
                }
                else "B"
              }
              else "O"
            }

            if (bilou && (nerTagString == "O" || lastTokNerCategory != nerTagString)) {
              lastTokNerBilou match {
                case "B" => lastTok.attr[NerTag].setCategory(s"U-$lastTokNerCategory")(null)
                case "I" => lastTok.attr[NerTag].setCategory(s"L-$lastTokNerCategory")(null)
                case _ => // otherwise do nothing
              }
            }
            lastTokNerBilou = tokNerBilou
            lastTokNerCategory = nerTagString
            if (lastTokNerBilou == "O") "O" else s"$lastTokNerBilou-$nerTagString"
          }


          else
            nerTagString
        }

        val token = new Token(currentSentence, startDocOffset.toInt, endDocOffset.toInt)
        token.attr += new TokenString(token, tokString)
        token.attr += new PennPosTag(token, posTagString)
        token.attr += {
          new BilouConllNerTag(token, nerTag)
        }
        //        println(s"${token.positionInSection}\t${token.positionInSentence}\t${token.string}\t${token.posTag.categoryValue}\t${token.nerTag.categoryValue}")

        lastTok = token
      }
    })
    src.close
    println("doc:"+doc.string + " \ttokens : " + doc.tokens.size)
    doc
  }

  /**
   * Deserialize a plain-text serialized Factorie document given a File corresponding to the
   * serialized document, this corresponds to older serialized documents which contain
   * document start and end offsets, and do not contain parse information.
   *
   * @param doc Factorie Document to deserialize into
   * @param file plaintext file to deserialize from
   * @param shortNer whether short or long ner tags (with/without BILOU) were serialized
   * @param bilou whether ner tags use BILOU or BIO
   * @return Factorie Document populated with serialized annotations
   */
  def deserializeFromFileOld(doc: Document, file: File, nerType: String = "tac", shortNer:Boolean = DEFAULT_SHORTNER, bilou: Boolean = DEFAULT_BILOU): Document = {
    //    doc.clearSections
    //    var currentSection: Paragraph = null
    var currentSentence: Sentence = null
    var lastTok: Token = null
    var lastTokNerBilou = "O"
    var lastTokNerCategory = "O"
    val src = io.Source.fromFile(file, encoding)
    src.getLines.foreach(line => {
      if (line.trim != "") {
        val fields = line.split("\t")
        val tokString = fields(0)
        val nerTagString = fields(1)

        //        val startFileOffset = Integer.parseInt(fields(2))
        //        val endFileOffset = Integer.parseInt(fields(3))
        val positionInSection = Integer.parseInt(fields(4))
        val positionInSentence = Integer.parseInt(fields(5))
        val startDocOffset = Integer.parseInt(fields(6))
        val endDocOffset = Integer.parseInt(fields(7))
        val posTagString = fields(8)

        // TODO doesn't deserialize sections because that's hard, but we don't need them
        //        if(positionInSection == 0) currentSection = new Paragraph(doc, startFileOffset, 0) // TODO this 0 is wrong
        //        if(positionInSentence == 0) currentSentence = new Sentence(currentSection)
        if (positionInSentence == 0) currentSentence = new Sentence(doc)

        //        if (startDocOffset == 0) doc.startOffset = startFileOffset

        // Approximately re-create BI(L)O(U) value if short ner tag (sans BILOU) was serialized
        val nerTag = {
          if(shortNer){
            val tokNerBilou = {
              if (nerTagString != "O") {
                if (lastTokNerCategory == nerTagString) {
                  lastTokNerBilou match {
                    case "O" => "B"
                    case "B" => "I"
                    case "I" => "I"
                    case _ => throw new Error("Something went wrong deserializing ner tags")
                  }
                }
                else "B"
              }
              else "O"
            }

            if (bilou && (nerTagString == "O" || lastTokNerCategory != nerTagString)) {
              lastTokNerBilou match {
                case "B" => lastTok.attr[NerTag].setCategory(s"U-$lastTokNerCategory")(null)
                case "I" => lastTok.attr[NerTag].setCategory(s"L-$lastTokNerCategory")(null)
                case _ => // otherwise do nothing
              }
            }
            lastTokNerBilou = tokNerBilou
            lastTokNerCategory = nerTagString
            if (lastTokNerBilou == "O") "O" else s"$lastTokNerBilou-$nerTagString"
          }
          else
            nerTagString
        }

        val token = new Token(currentSentence, startDocOffset, endDocOffset)
        token.attr += new TokenString(token, tokString)
        token.attr += new PennPosTag(token, posTagString)
        token.attr += {
          new BilouConllNerTag(token, nerTag)
        }
        //        println(s"${token.positionInSection}\t${token.positionInSentence}\t${token.string}\t${token.posTag.categoryValue}\t${token.nerTag.categoryValue}")

        lastTok = token
      }
    })
    println(s"Loaded document with ${doc.tokenCount} tokens and ${doc.sentenceCount} sentences")
    src.close
    doc
  }

  private def addDepInfo(s: Sentence, depInfoSeq: Seq[(Int,Int,String)]): Unit = {
    //assert(depInfoSeq.map(_._1) == Seq.tabulate(depInfoSeq.length)(i => i), "Token indices: "+depInfoSeq.map(_._1).mkString(" ")) // Assert that we have an entry for each token index, in order
    val tree = new ParseTree(s, depInfoSeq.map(_._2), depInfoSeq.map(_._3))
    s.attr += tree
  }
}