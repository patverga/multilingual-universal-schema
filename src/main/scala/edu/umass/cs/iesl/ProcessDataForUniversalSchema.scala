package edu.umass.cs.iesl

import java.io.{File, PrintWriter}

import cc.factorie.app.nlp.Document
import cc.factorie.la.DenseTensor1
import edu.umass.cs.iesl.entity_embeddings.EntityEmbeddingOpts
import edu.umass.cs.iesl.entity_embeddings.data_structures._
import edu.umass.cs.iesl.entity_embeddings.data_structures.data_stores.{EmbeddingCollection, SurfaceFormDB, TypeDB}
import edu.umass.cs.iesl.entity_embeddings.linking.LogisticRegressionTrainedLinker
import edu.umass.cs.iesl.entity_embeddings.util.FileIO

import scala.StringBuilder

/**
 * Created by pv on 3/5/15.
 */


object ProcessDataForUniversalSchema
{
  def main(args : Array[String]) {
    val opts = new MultilingualUniversalSchemaOpts
    opts.parse(args)

    val linker = initializeLinker(opts)
    val mentionFinder = if (opts.language.value == "es") SpanishNERMentionFinder else EnglishNERMentionFinder

    val elDocs = loadData(opts)
    val result = processELDocs(elDocs, mentionFinder, linker)

    println(result)

    if (opts.outputFileName.wasInvoked) {
      exportRelations(opts.outputFileName.value, result)
    }
  }

  def exportRelations(outputFile :String, result: String): Unit = {
    val printWriter = new PrintWriter(outputFile)
    printWriter.write(result + "\n")
    printWriter.close()
  }

  def processELDocs(elDocs : Seq[ELDocument], mentionFinder: MultilingualNERMentionFinder,
                            linker : LogisticRegressionTrainedLinker): String = {
    // load data and process each doc in parallel
    elDocs.par.zipWithIndex.map { case (elDoc, i) =>
      println(s"Processing document $i")
      // Convert to a factorie document
      val fDoc = elDoc.toFactorieDocument

      mentionFinder.process(fDoc)
      linker.process(fDoc)
      LogPatternsRelations.process(fDoc)

      formatRelationsForExport(fDoc)
    }.mkString("")
  }

  def loadData(opts : MultilingualUniversalSchemaOpts) : Seq[ELDocument] ={
    // read in input text
    print("Reading in text...")
    def file2String(f: File): String = {
      val inputSource = scala.io.Source.fromFile(f, "ISO-8859-1") //UTF-8")
      val text = inputSource.getLines mkString "\n"
      inputSource.close()
      text
    }
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

  def initializeLinker(opts : MultilingualUniversalSchemaOpts): LogisticRegressionTrainedLinker ={

    // Load the typeDB
    val typeDB = TypeDB.fromCMDOptions(opts)
    println("Loading Embeddings")
    // Load the embeddings
    val embeddingCollection = EmbeddingCollection.fromCMDOptions(opts)
    // Define the entity linker
    val features = opts.features.value.map(FeatureType.fromIsoString).toList
    val weights = new DenseTensor1(FileIO.readFeatureWeights(opts.featureWeightsFilename.value.head))
    val contextWindowSize = (opts.window.value, 10000, 10000)

    // Load the surface form database
    val surfaceFormDB = SurfaceFormDB.fromCMDOptions(opts)
    new LogisticRegressionTrainedLinker(features, embeddingCollection, surfaceFormDB,
      typeDB, null, contextWindowSize, opts.caseSensitiveWords.value, opts.caseSensitiveMentions.value, weights = weights)

  }
  
  def formatRelationsForExport(doc: Document): String = {
    val sb = new StringBuilder
    val relationMentionList = doc.attr[EntityLinkedRelationMentionList]
    relationMentionList.foreach(rm => {
      rm._relations.foreach(r => {
        val e1 = Slug.unSlug(rm.arg1.entitySlug)
        val e2 = Slug.unSlug(rm.arg2.entitySlug)
        val fbid1 = FreebaseWikiBiMap(WikipediaId(e1))
        val fbid2 = FreebaseWikiBiMap(WikipediaId(e2))
        if (fbid1 != None && fbid2 != None) {
//          sb.append(s"$e1\t${rm.arg1.toString()}\t") // id1 nertag
//          sb.append(s"$e2\t${rm.arg2.toString()}\t") // id1 nertag
          sb.append(s"$e1\t${fbid1.get.value}\t") // id1 nertag
          sb.append(s"$e2\t${fbid2.get.value}\t") // id2 nertag
//          sb.append(s"${Slug.unSlug(rm.arg1.span.string)}\t${Slug.unSlug(rm.arg2.span.string)}\t") // string1 string2
//          sb.append(s"${doc.name}\t") // docid
//          sb.append(s"${rm.arg1.span.head.stringStart}-${rm.arg1.span.last.stringEnd}:") // first mention offsets
//          sb.append(s"${rm.arg2.span.head.stringStart}-${rm.arg2.span.last.stringEnd}:") // second mention offsets
          //        sb.append(s"${rm.arg1.span.sentence.head.stringStart}-${rm.arg2.span.sentence.last.stringEnd}\t") // whole sentence offsets
          sb.append(s"${r.provenance}\t") // evidence
          sb.append("1.0\n")
//          sb.append("\n")
        } else {
          if (fbid1 == None) print(s"Could not link $e1.\t")
          if (fbid2 == None) print(s"Could not link $e2.")
          println()
        }
      })
    })
    sb.toString()
  }

}


class MultilingualUniversalSchemaOpts extends EntityEmbeddingOpts{
  val inputFileName = new CmdOption[String]("input-filename", "inputFileName", "FILENAME", "The filename of the raw text input data.")
  val inputDirName = new CmdOption[String]("input-directory", "inputDirName", "DIRNAME", "A directory containing different text files.")
  val outputFileName = new CmdOption[String]("output-filename", "outputFileName", "FILENAME", "File to output results to.")
}
