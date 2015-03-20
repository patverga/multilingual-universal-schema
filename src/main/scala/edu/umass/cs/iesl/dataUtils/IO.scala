package edu.umass.cs.iesl.dataUtils

import java.io.{File, FileOutputStream, PrintWriter}
import edu.umass.cs.iesl.entity_embeddings.data_structures.{DocLanguage, ELDocument}
import edu.umass.cs.iesl.process.MultilingualUniversalSchemaOpts
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorOutputStream

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

