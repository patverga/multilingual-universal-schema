package edu.umass.cs.iesl

import java.io.PrintWriter

import cc.factorie.app.nlp.Document
import cc.factorie.la.DenseTensor1
import edu.umass.cs.iesl.entity_embeddings.EntityEmbeddingOpts
import edu.umass.cs.iesl.entity_embeddings.data_structures._
import edu.umass.cs.iesl.entity_embeddings.data_structures.data_stores.{EmbeddingCollection, SurfaceFormDB, TypeDB}
import edu.umass.cs.iesl.entity_embeddings.linking.LogisticRegressionTrainedLinker
import edu.umass.cs.iesl.entity_embeddings.util.FileIO

/**
 * Created by pv on 3/5/15.
 */


object UniversalSchemaPipeline extends App
{
  val opts = new MultilingualUniversalSchemaOpts
  opts.parse(args)

  // read in input text
  print("Reading in text...")
  val inputText = if (opts.inputFileName.wasInvoked) {
    val inputSource = scala.io.Source.fromFile(opts.inputFileName.value, "ISO-8859-1") //UTF-8")
    val text = inputSource.getLines mkString "\n"
    inputSource.close()
    text
  }
  else // use some example text if input not given
    "The last time I went to Boston, I visited the home of Paul Revere in Quincy. I also visited the MFA and ate lunch with my friend at Harvard."
  println("done.")

  // Document Representation for Entity linking
  val elDoc = ELDocument("test", inputText, lang=English)
  // Convert to a factorie document
  val fDoc = elDoc.toFactorieDocument

  EnglishNERMentionFinder.process(fDoc)
  val linker = initializeLinker(opts)
  linker.process(fDoc)
  SlotFillingLogPatternRelationMentionFindingComponent.process1(fDoc)

  val result = formatDoc(fDoc)
  println(result)
  if (opts.outputFileName.wasInvoked) {
    val printWriter = new PrintWriter(opts.outputFileName.value)
    printWriter.write(result + "\n")
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
  
  def formatDoc(doc: Document): String = {
    val sb = new StringBuilder
    val relationMentionList = doc.attr[RelationMentionList2]
    relationMentionList.foreach(rm => {
      rm._relations.foreach(r => {
        sb.append(s"${Slug.unSlug(rm.arg1.entitySlug)}\t") //${rm.arg1.span.head.nerTag.baseCategoryValue}\t") // id1 nertag
        sb.append(s"${Slug.unSlug(rm.arg2.entitySlug)}\t") //${rm.arg2.span.head.nerTag.baseCategoryValue}\t") // id2 nertag
        sb.append(s"${Slug.unSlug(rm.arg1.span.string)}\t${Slug.unSlug(rm.arg2.span.string)}\t") // string1 string2
        sb.append(s"${doc.name}\t") // docid
        sb.append(s"${rm.arg1.span.head.stringStart}-${rm.arg1.span.last.stringEnd}:") // first mention offsets
        sb.append(s"${rm.arg2.span.head.stringStart}-${rm.arg2.span.last.stringEnd}:") // second mention offsets
//        sb.append(s"${rm.arg1.span.sentence.head.stringStart}-${rm.arg2.span.sentence.last.stringEnd}\t") // whole sentence offsets
        sb.append(s"${r.provenance}") // evidence
        sb.append("\n")
      })
    })
    sb.toString()
  }

}

object RelationComponents extends ChainedNLPComponent {
  lazy val components = Seq(
    DeterministicSubstringNerCorefComponent,
    SlotFillingLogPatternRelationMentionFindingComponent
  )
}

class MultilingualUniversalSchemaOpts extends EntityEmbeddingOpts{
  val inputFileName = new CmdOption[String]("input-filename", "inputFileName", "FILENAME", "The filename of the raw text input data.")
  val outputFileName = new CmdOption[String]("output-filename", "outputFileName", "FILENAME", "File to output results to.")

}
