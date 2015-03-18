package edu.umass.cs.iesl

import java.io.{PrintWriter, FileOutputStream, File}

import edu.umass.cs.iesl.entity_embeddings.data_structures.{DocLanguage, Spanish, ELDocument}
import edu.umass.cs.iesl.entity_embeddings.embedding.EntityResolver
import edu.umass.cs.iesl.entity_embeddings.linking.LogisticRegressionTrainedLinker
import edu.umass.cs.iesl.entity_embeddings.load.LoadWikipediaArticlesCrossWikis
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorOutputStream

import scala.collection.mutable.ArrayBuffer

/**
 * Created by pv on 3/13/15.
 */

object IO
{
  def loadPlainTextTestData(opts : MultilingualUniversalSchemaOpts) : Seq[ELDocument] ={
    // read in input text
    print("Reading in text...")
    if (opts.inputDirName.wasInvoked){
      new File(opts.inputDirName.value).listFiles().toSeq.map(f => {
        ELDocument(f.getName, file2String(f), lang = DocLanguage.fromIsoString(opts.language.value))
      })
    }
    else {
      val inputText = if (opts.inputFileName.wasInvoked) {
        file2String(new File(opts.inputFileName.value))
      } // use some example text if input not given
      else if (opts.language.value == "es")
        "La última vez que fui a Boston, que visitó la casa de Paul Revere en Quincy. También visitó la MFA y almorzaba con mi amigo en Harvard."
      else
        "The last time I went to Boston, I visited the home of Paul Revere in Quincy. I also visited the MFA and ate lunch with my friend at Harvard."
      println("done.")

      println("Setting up ElDoc...")
      // Document Representation for Entity linking
      Seq(ELDocument("test", inputText, lang = DocLanguage.fromIsoString(opts.language.value)))
    }
  }

  /**
   * Get the stirng contents of a file
   * @param f file object
   * @param encoding encoding type, default ISO
   * @return string content of file
   */
  def file2String(f: File, encoding : String = "ISO-8859-1"): String = {
    val inputSource = scala.io.Source.fromFile(f, encoding) //UTF-8")
    val text = inputSource.getLines mkString "\n"
    inputSource.close()
    text
  }

  /**
   * export a string to file
   * @param outputFile file location as string
   * @param exportString string to export
   * @param append append to existing file - default false
   * @param compress compres in bzip2 - default false
   */
  def exportStringToFile(outputFile :String, exportString: String, append : Boolean = false, compress : Boolean = false): Unit = {
    val outputStream = new FileOutputStream(outputFile, append)
    val printWriter = new PrintWriter(if (compress) new BZip2CompressorOutputStream(outputStream) else outputStream)
    printWriter.write(exportString + "\n")
    printWriter.close()
  }
}

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
  val linker = ProcessDataForUniversalSchema.initializeLinker(opts)
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
        LogPatternsRelations.process(fDoc)
        val result = ProcessDataForUniversalSchema.formatRelationsForExport(fDoc)
        IO.exportStringToFile(s"processed_wikis/${opts.language.value}/${j}_${elDoc.title}", result, append = true)
      }
      batch = new ArrayBuffer[(Int, ELDocument)]
    }
  }
}

object ExportWikis extends App
{
  val filesPerDir = 128
  val opts = new MultilingualUniversalSchemaOpts
  opts.parse(args)
  val lang = DocLanguage.fromIsoString(opts.language.value)
  val entityResolver = EntityResolver.fromCMDOptions(opts, lang)
  val wikiProcessor = new LoadWikipediaArticlesCrossWikis(lang, entityResolver)
  val wikiIterator = wikiProcessor.fromCompressedFilename(opts.inputFileName.value)

  println(s"Exporting wiki data at ${opts.inputFileName.value}")
  var dirName = "0"
  var outDir = s"wikis/${opts.language.value}/$dirName/"
  var i = 0
  while (wikiIterator.hasNext){
    val wikiArticle = wikiIterator.next()
    val outFile = s"${wikiArticle.title.replaceAll(" ","_")}.bz"
    if (i % filesPerDir == 0) {
      dirName = (dirName.toInt + 1).toString
      outDir = s"wikis/${opts.language.value}/$dirName/"
      val outDirFile = new File(outDir)
      if (!outDirFile.exists()) outDirFile.mkdirs()
      println(i, outFile)
    }
    IO.exportStringToFile(s"$outDir/$outFile", wikiArticle.rawDocumentText, append = false, compress = true)
    i+= 1
  }
}