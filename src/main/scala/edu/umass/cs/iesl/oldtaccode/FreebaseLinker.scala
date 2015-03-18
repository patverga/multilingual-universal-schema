package edu.umass.cs.iesl.oldtaccode

import cc.factorie.app.nlp.{Token, Sentence, Document}
import org.apache.lucene.index.IndexReader
import cc.factorie.util.DefaultCmdOptions
import java.io.{FileWriter, BufferedWriter, File}
import org.apache.lucene.store.FSDirectory
import scala.io.Source
import scala.Some


/**
 * Links canonical surface strings to FreebaseIds, if possible
 * @param threshold if the most prominent matching wikipedia article exceeds this threshold select it without recourse to resolver
 * @param resolver a function that selects one of a group of possible rows to link to a given surface form
 */
class FreebaseLinker[HT <: HashType](kb:FreebaseWikiKB[HT], val threshold:Double, resolver:((Seq[KBRow], Document) => Option[KBRow])) {
  def linkString(surfaceString:String, entType:EntityType, doc:Document):Option[FreebaseId] = {

    val rows = kb(surfaceString, entType)
    println("rows: "+ rows.size)
    if (rows.size == 0) {
      None
    } else if(rows.size == 1 || rows.head.proportion >= threshold) {
      Some(rows.head.freebaseId)
    } else {
      resolver(rows, doc).map{_.freebaseId}
    }
  }

  def process(doc: Document) = {
    doc.coref.entities.foreach { withinDocEnt =>
      withinDocEnt.mentions.toSeq.sortBy(ment => - ment.string.length).foldLeft[Option[FreebaseId]](None) {
        case (None, ment) => {
          if(kb.useTypes) {
            println("mention: " + ment.phrase.string)
            val tmp = EntityType.fromTacString(ment.phrase.headToken.nerTag.baseCategoryValue)
            //            println(tmp.size)
            tmp.flatMap{
              entType => val res = linkString(ment.string, entType, doc)
                res
            }
          } else {
            linkString(ment.string, null.asInstanceOf[EntityType], doc) // I know, I know
          }
        }
        case (someFID, _) => someFID // we know this will be 'some' because we handled the 'none' case above
      } match {
        case Some(fId) => withinDocEnt.attr += fId
        case None => withinDocEnt.attr += SurfaceFormId(withinDocEnt.canonicalMention.string)
      }
    }
    doc
  }
}

class LuceneFreebaseLinker[HT <: HashType](kb:FreebaseWikiKB[HT], reader:IndexReader, threshold:Double, tfidfCutoff:Int, resolveThreshold:Double)
  extends FreebaseLinker[HT](kb, threshold, new WikipediaResolver(reader, tfidfCutoff, resolveThreshold).retrieveBestMatch)


class FreebaseLinkerOpts extends DefaultCmdOptions {
  val threshold = new CmdOption("threshold", 0.9, "Double [0,1]", "the threshold above which to automatically link")
  val f2wFile = new CmdOption("f2w", "", "FILE", "the file containing freebase to wiki mappings")
  val expansions = new CmdOption("expansion-file", "", "FILE", "the file containing expansions for wikipedia surface forms")
  val indexDir = new CmdOption("index-dir", "", "DIRECTORY", "The directory that stores the wiki index")
  val tfidfCutoff = new CmdOption("tfidf-size", 50, "INT", "The top-n tokens (as ranked by tfidf) to link against")
  val resolverCutoff = new CmdOption("resolver-cutoff", 0.3, "Double [0, 1]", "The cosine similarity beneath which to reject a retrieved wiki page.")
}

object FreebaseLinker {

  def default = {
    val f2wMap = new FreebaseWikiBiMap(new File("/iesl/canvas/beroth/tac/data/freebase_to_wiki_types.tsv"))

    val kb = new FreebaseWikiKB(f2wMap, new File("/iesl/canvas/beroth/tac/data/relationfactory_models/expansion/enwiki.linktext.counts"), true, StringHash)

    val reader = IndexReader.open(FSDirectory.open(new File("/iesl/canvas/proj/tackbp2014/indices/linking/")))

    val linker = new LuceneFreebaseLinker(kb, reader, 0.9, 50, 0.3)
    linker
  }

  def makeDoc(tokens:Seq[String]):Document = {
    val doc = new Document(tokens.mkString(" "))
    val sent = new Sentence(doc)
    tokens.foreach{ tokenString => new Token(sent, tokenString)}
    doc
  }

  //def makeDoc(tokens:Seq[String]):Document = tokens.foldLeft(new Document){case (doc, token) => doc.asSection += token; doc}

  def linkRowWithDefaultSettings(surfaceForm:String, tacString:String, context:Seq[String], linker: FreebaseLinker[_] = default):Option[String] = {
    //    val linker = default
    EntityType.fromTacString(tacString) match {
      case Some(entType) =>
        val doc = makeDoc(context)
        linker.linkString(surfaceForm, entType, doc).map(_.value)
      case None => printf("Failed to link surface form %s because I couldn't find an entity for the tac entity string %s\n", surfaceForm, tacString); None
    }
  }

  def main(args:Array[String]) {
    val opts = new FreebaseLinkerOpts {
      val surfaceFormsFile = new CmdOption("surface-forms", "", "FILE", "File that contains lines of surface form, entity type, and optional context, each separated by tabs.")
      var outputFile = new CmdOption("output-file", "", "FILE", "file to write linking responses.")
      var alwaysId = new CmdOption("always-id", false, "FILE", "'true': Always return an id - if not linkable fall back to surface form. 'false': empty line if not linkable")
    }
    opts.parse(args)

    val f2wMap = new FreebaseWikiBiMap(new File(opts.f2wFile.value))
    println("built WikiBiMap")
    val kb = new FreebaseWikiKB(f2wMap, new File(opts.expansions.value), true, StringHash)
    println("built FreebaseWikiKB")
    val reader = IndexReader.open(FSDirectory.open(new File(opts.indexDir.value)))
    println("built IndexReader")

    val linker = new LuceneFreebaseLinker[StringHash.type](kb, reader, opts.threshold.value, opts.tfidfCutoff.value, opts.resolverCutoff.value)
    val noContextLinker = new FreebaseLinker(kb, opts.threshold.value, {(_, _) => None.asInstanceOf[Option[KBRow]]})
    println("built Linker")
    val writer = new BufferedWriter(new FileWriter(opts.outputFile.value))

    Source.fromFile(opts.surfaceFormsFile.value).getLines().foreach { line =>
      val split = line.split("\t")

      if(split.length == 2 || split.length == 3) {
        val l = if (split.length == 2) noContextLinker else linker
        val context = if (split.length == 2) null.asInstanceOf[Seq[String]] else split(2).split(" ").toSeq
        val surfaceForm = split(0)
        val entString = split(1)
        EntityType.fromTacString(entString) match {
          case Some(entType) =>
            l.linkString(surfaceForm, entType,  makeDoc(context)) match {
              case Some(fId) =>
                f2wMap(fId) match {
                  case Some((WikipediaId(wIdString), _)) =>
                    writer.write("%s\t%s".format(fId.value, wIdString))
                    writer.newLine()
                  case None =>
                    writer.write("%s".format(fId.value))
                    writer.newLine()
                }
              case None =>
                if (opts.alwaysId.value) {
                  writer.write("%s\t%s".format(surfaceForm, surfaceForm))
                }
                writer.newLine()
            }
          case None =>
            if (opts.alwaysId.value) {
              writer.write("%s\t%s".format(surfaceForm, surfaceForm))
            }
            writer.newLine()
        }
      } else {
        println("Error: Malformed line: %s Expected 2 or 3 tab-separated columns, but trying to split gave me: %s".format(line, split))
      }
    }
    writer.flush()
    writer.close()

  }
}