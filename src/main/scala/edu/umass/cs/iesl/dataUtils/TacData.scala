package edu.umass.cs.iesl.dataUtils

import java.io.File

import cc.factorie.app.nlp.load.TACDocTypes.TACDocumentType
import cc.factorie.app.nlp.{Document, CompoundDocumentAnnotator}
import cc.factorie.app.nlp.load._
import cc.factorie.app.nlp.segment.DeterministicNormalizingHtmlTokenizer
import cc.factorie.util.DefaultCmdOptions
import edu.umass.cs.iesl.entity_embeddings.data_structures.{DocLanguage, Spanish, ELDocument}
import edu.umass.cs.iesl.process._


/**
 * @author John Sullivan
 */
object TacAnnotator extends Serializable {

  val annos = Seq(
    DeterministicNormalizingHtmlTokenizer,   // tokenize, normalize
    TACSectionalizer                       // remove sgml
  )
  val compound = new CompoundDocumentAnnotator(annos)
}

object TACPipelineTest {

  def main(args:Array[String]) {
    val opts = new MultilingualUniversalSchemaOpts
    opts parse args

    val lang = DocLanguage.fromIsoString(opts.language.value)

    val tacFiles = new TacFileIterator(new File(opts.inputFileName.value))

    println("Processing files...")
    val elDocs = tacFiles.map ( serDoc =>
    {
      val doc = new Document(serDoc.docString).setName(serDoc.id)
      doc.attr += TACDocumentType.fromFilePath(new File("newswire"))
      doc.annotators += classOf[TACDocumentType] -> classOf[TACDocumentType]

      TacAnnotator.compound.process(doc)
      new ELDocument(serDoc.id, doc.sections(1).string, lang = lang)
    }).toSeq

    println("Mention Finding and Entity Linking...")
    val mentionFinder = if (lang == Spanish) SpanishNERMentionFinder else EnglishNERMentionFinder
    ExactStringFreebaseLinkedProcesser.batchProcess(opts, mentionFinder, ExactStringFreebaseLink, elDocs)
  }
}



