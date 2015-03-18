package edu.umass.cs.iesl.oldtaccode

import java.io.{BufferedReader, FileReader, StringReader, File}
import scala.io.Source
import org.apache.lucene.index.{Term, IndexReader, IndexWriterConfig, IndexWriter}
import org.apache.lucene.store.FSDirectory
import org.apache.lucene.util.Version
import org.apache.lucene.document.Field
import org.apache.lucene.analysis.WhitespaceAnalyzer
import cc.factorie.app.nlp.{Document => FacDocument}
import cc.factorie.app.nlp.segment.{PlainTokenNormalizer, DeterministicTokenizer}
import org.apache.lucene.search.similar.{MoreLikeThisQuery, MoreLikeThis}
import org.apache.lucene.search._
import cc.factorie.util.CmdOptions
import java.net.URLDecoder
import scala.collection.mutable
import org.apache.commons.lang3.StringEscapeUtils
import scala.concurrent.ExecutionContext.Implicits.global
import org.apache.lucene.analysis.standard.StandardAnalyzer
import scala.reflect.ClassTag
import org.apache.lucene.queryParser.QueryParser
import scala.collection.JavaConverters._
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import scala.Some
import cc.factorie._
import cc.factorie.variable.BagOfWords

/**
 * @author John Sullivan
 */
class FreebaseWikiBiMap(f2wFile:File) {

  //todo let this get garbage collected?
  val rows = Source.fromFile(f2wFile).getLines().map { line =>
    val Array(fId, wId, fType) = line.split("\t")
    (FreebaseId(fId), WikipediaId(wId), EntityType.fromFreebaseString(fType))
  }.toSeq

  private val f2w = rows.groupBy(_._1).mapValues{ wRows =>
    wRows.head._2 -> wRows.flatMap(_._3).toSet
  }

  private val w2f = rows.groupBy(_._2).mapValues{ fRows =>
    fRows.head._1 -> fRows.flatMap(_._3).toSet
  }

  def apply(fId:FreebaseId):Option[(WikipediaId, Set[EntityType])] = f2w.get(fId)

  def apply(wId:WikipediaId):Option[(FreebaseId, Set[EntityType])] = w2f.get(wId)
}

object KBRow {
  def normalize(rows:Seq[KBRow]):Seq[KBRow] = {
    val denom = rows.map(_.numOccurrences).sum
    rows.foreach(row => row.proportion = row.numOccurrences / denom)
    rows
  }
}

case class KBRow(numOccurrences:Double, wikiSlug:WikipediaId, freebaseId:FreebaseId) {
  var proportion = null.asInstanceOf[Double]
}

class TopN(n:Int, freebaseWikiBiMap:FreebaseWikiBiMap) {
  private val elems = mutable.HashSet[(String, Double)]()
  def +=(tup:(String, Double)) {
    elems += tup
  }

  def asSeq:Seq[KBRow] = KBRow.normalize(elems.toSeq.sortBy(e => e._2 * -1).take(n).map{ case (fIdString, count) =>
    val fId = FreebaseId(fIdString)
    // we know that freebaseWikiBiMap contains fId because of the filtering step in FreebaseWikiKB that constructs TopNs.
    KBRow(count, freebaseWikiBiMap(fId).get._1, fId)
  })

  override def toString = asSeq.toString()
}


sealed trait HashType {
  type Value
}
case object StringHash extends HashType {type Value = String}
case object IntHash extends HashType {type Value = Int}

class FreebaseWikiKB[HT <: HashType](freebaseWikiBiMap:FreebaseWikiBiMap, wikiExpFile:File, val useTypes:Boolean, ht:HT, n:Int = 5) {


  def makeKey(surfaceForm:String, entType:EntityType):HT#Value = {
    val res = ht match {
      case _:StringHash.type => if(useTypes) surfaceForm + entType.tacValue else surfaceForm
      case _:IntHash.type => if(useTypes) surfaceForm.hashCode ^ entType.tacValue.hashCode else surfaceForm.hashCode
    }
    res.asInstanceOf[HT#Value]
  }

  private val ord = new scala.math.Ordering[(String, Double)] {
    def compare(x: (String, Double), y: (String, Double)) = (x._2 compareTo y._2) * -1
  }

  private val surfaceWikiMap = mutable.HashMap[HT#Value, TopN]().withDefault(_ => new TopN(n, freebaseWikiBiMap))


  Source.fromFile(wikiExpFile).getLines().foreach{ line =>

    line.split(" ").toList match {
      case count :: wikiString :: sF =>
        val surfaceForm = sF.mkString(" ")
        val wId = WikipediaId(wikiString)

        freebaseWikiBiMap(wId).map { case(fId, entTypes) =>
          entTypes.map { entType =>

            val topN = surfaceWikiMap(makeKey(surfaceForm, entType))
            topN.+=(fId.value -> count.toDouble)
            surfaceWikiMap(makeKey(surfaceForm, entType)) = topN
          }
        }
      case l => println("Discarded malformed line: %s from wiki expansion file %s".format(line, wikiExpFile.getAbsolutePath))
    }
  }

  def apply(surfaceString:String, entType:EntityType):Seq[KBRow] =
    surfaceWikiMap.get(makeKey(surfaceString, entType)).map(_.asSeq).getOrElse(Seq.empty[KBRow])



}


class WikipediaIndexerOpts extends CmdOptions {
  val indexDir = new CmdOption("index-dir", "", "directory", "directory into which to Index")
  val f2wFile = new CmdOption("f2w", "", "FILE", "the file containing freebase to wiki mappings")
  val wikiDir = new CmdOption("wiki-dir", "", "DIRECTORY", "The directory of SGML wiki docs")
}

object WikipediaIndexer {

  val WIKI_TEXT = "WIKI_TEXT"
  val WIKI_TITLE = "WIKI_TITLE"


  def main(args:Array[String]) {
    val opts = new WikipediaIndexerOpts
    opts.parse(args)
    println("Building wiki map")
    val f2wMap = new FreebaseWikiBiMap(new File(opts.f2wFile.value))
    println("Built wiki map of size " + f2wMap.rows.size)

    val filesIter = new File(opts.wikiDir.value).listFiles().toIterator

    buildIndex(f2wMap, filesIter, new File(opts.indexDir.value))

    println("Finished building index")
  }



  def buildIndex(freebaseWikiMap:FreebaseWikiBiMap, files:Iterator[File], indexDir:File) {
//    var docCount = 0
//    var totalCount = 0
//    val startTime = System.currentTimeMillis()
//
//    files.flatMap{f =>
//      val docs = BlikiSGMReader.processDocs(f)
//      totalCount += docs.size
//      docs.par.filter(doc => freebaseWikiMap(WikipediaId(doc.name)).isDefined).seq//.map(DeterministicTokenizer.process _ andThen PlainTokenNormalizer.process).seq
//    }.foldLeft(
//        new IndexWriter(
//          FSDirectory.open(indexDir),
//          new IndexWriterConfig(Version.LUCENE_36,
//            new StandardAnalyzer(Version.LUCENE_36)).setRAMBufferSizeMB(2048))) { case (indexWriter, doc) =>
//      if (docCount % 1000 == 0) {
//        println("processed %d docs, of which we've matched and indexed %d in %.3f secs".format(totalCount, docCount, (System.currentTimeMillis() - startTime)/1000.0))
//      }
//      //totalCount += 1
//      val articleName = doc.name
//      val articleText = doc.string //.tokens.map(_.string).mkString(" ")
//      freebaseWikiMap(WikipediaId(articleName)) match {
//        case Some((fId, _)) =>
//          docCount += 1
//          indexWriter addDocument
//            Seq((WIKI_TITLE, articleName).toField(Field.Store.YES, Field.Index.NOT_ANALYZED),
//              (WIKI_TEXT, articleText).toField(Field.Store.YES, Field.Index.ANALYZED)).toDocument
//        case None => ()
//      }
//      indexWriter
//    }.close()
  }
}

object BlikiSGMReader {
  val docIdRegex = """<DOC ID="([^"]+)">""".r
  val docEndRegex = "</DOC>"

  def processDocs(file:File):Seq[FacDocument] = {
    println("loading file: " + file.getName)
    var docString = new mutable.StringBuilder
    var docId = null.asInstanceOf[String]
    val docs = mutable.ArrayBuffer[FacDocument]()
    Source.fromFile(file).getLines().foreach{
      case line if line.startsWith("<DOC ID=") =>
        //val id = docIdRegex.findFirstMatchIn(line).get.toString()
        val docIdRegex(id) = line
        docId = URLDecoder.decode(id, "UTF-8").replace(' ','_')
        docString append line
      case line if line.startsWith(docEndRegex) =>
        docString.append(line)
        docs += new FacDocument(docString.toString()).setName(docId)
        docString = new mutable.StringBuilder
        docId = null.asInstanceOf[String]
      case line =>
        docString.append(StringEscapeUtils.escapeHtml4(line))
    }
    docs
  }
}

class WikipediaResolver(reader:IndexReader, tfidfCutoff:Int = 50, similarityThreshold:Double = 0.3) {

  val searcher = new IndexSearcher(reader)
  val mlt = new MoreLikeThis(reader)
  val WIKI_TEXT = "WIKI_TEXT"
  val WIKI_TITLE = "WIKI_TITLE"


  def generateTfIdfs(tokens:Seq[String], ir:IndexReader, field:String, n:Int):Seq[(String, Double)] = {

    val words = tokens.groupBy(identity).mapValues(_.size).toSeq.sortBy(w => w._2 * -1)
    val numDocs = ir.numDocs()

    val res = words.map{ case (word, tf)  =>
      val docFreq = ir.docFreq(new Term(field, word))
      val idf = mlt.getSimilarity.idf(docFreq, numDocs)
      word -> tf.toDouble * idf
    }.sortBy(tup => -tup._2).take(n)
    //println("Found tfIdfs: %s".format(res))
    res

  }

  def tokenizeDocString(docString:String):Seq[String] = {
    val ts = mlt.getAnalyzer.reusableTokenStream(WIKI_TEXT, new StringReader(docString))

    val tokens = mutable.ArrayBuffer[String]()

    val termAttr = ts.addAttribute(classOf[CharTermAttribute])
    ts.reset()
    while(ts.incrementToken()) {tokens += termAttr.toString }
    ts.end()
    ts.close()

    tokens.toSeq
  }

  def retrieveBestMatch(rows:Seq[KBRow], doc:FacDocument):Option[KBRow] = {

    //todo this threshold should be settable
    //we can use take here because it is stipulated elsewhere that the KBRows are ordered by prominence
    val articleFilter = rows.take(100).map(r => new TermQuery(new Term(WIKI_TITLE, r.wikiSlug.value))).foldLeft(new BooleanQuery()) { case (boolQ, termQ) =>
      boolQ.add(termQ, BooleanClause.Occur.SHOULD)
      boolQ
    }
    articleFilter.setMinimumNumberShouldMatch(1)


    val docString = doc.string

    val docBag = new BagOfWords(null, generateTfIdfs(tokenizeDocString(docString), reader, WIKI_TEXT, tfidfCutoff).toMap)

    val bestDocs = searcher.search(articleFilter, 10).scoreDocs

    val docsAndBags = bestDocs.map { d =>
      val tfidfs = generateTfIdfs(tokenizeDocString(searcher.doc(d.doc).getFieldable(WIKI_TEXT).stringValue()), reader, WIKI_TEXT, tfidfCutoff)
      searcher.doc(d.doc).getFieldable(WIKI_TITLE).stringValue() -> new BagOfWords(null, tfidfs.toMap)
    }

    if(docsAndBags.size != 0) {
      val (bestDocId, bag) = docsAndBags.maxBy(_._2 cosineSimilarity docBag)
      val score = bag cosineSimilarity docBag
      rows.collectFirst{ case row @ KBRow(_, WikipediaId(wikiSlug), _) if score >= similarityThreshold && bestDocId == wikiSlug =>
        row
      }
    } else {
      rows.headOption
    }
  }
}

























