package edu.umass.cs.iesl.dataUtils

import java.io._
import edu.umass.cs.iesl.entity_embeddings.data_structures.{Spanish, DocLanguage, ELDocument}
import edu.umass.cs.iesl.process.MultilingualUniversalSchemaOpts
import org.apache.commons.compress.compressors.CompressorStreamFactory
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorOutputStream

/**
 * Created by pv on 3/13/15.
 */

object IO
{
  def loadPlainTextTestData(opts : MultilingualUniversalSchemaOpts) : Seq[ELDocument] ={
    // read in input text
    print("Reading in text...")
    val lang = DocLanguage.fromIsoString(opts.language.value)
    if (opts.inputDirName.wasInvoked){
      subFiles(new File(opts.inputDirName.value), lang)
    }
    else {
      val inputText = if (opts.inputFileName.wasInvoked) {
        file2String(new File(opts.inputFileName.value))
      } // use some example text if input not given
      else if (lang == Spanish)
        "La última vez que fui a Boston, que visitó la casa de Paul Revere en Quincy. También visitó la MFA y almorzaba con mi amigo en Harvard."
      else
        "The last time I went to Boston, I visited the home of Paul Revere in Quincy. I also visited the MFA and ate lunch with my friend at Harvard."
      println("done.")

      println("Setting up ElDoc...")
      // Document Representation for Entity linking
      Seq(ELDocument("test", inputText, lang = lang))
    }
  }

  def subFiles(f : File, lang : DocLanguage) : Seq[ELDocument] = {
    if (f.isDirectory)
      f.listFiles().toSeq.flatMap(subFiles(_, lang))
    else
      Seq(ELDocument(f.getName, file2String(f), lang = lang))
  }

  /**
   * Get the stirng contents of a file
   * @param f file object
   * @param encoding encoding type, default ISO
   * @return string content of file
   */
  def file2String(f: File, encoding : String = "UTF-8")
  : String = {
    val fin = new FileInputStream(f)
    val bis = new BufferedInputStream(fin)
    val stream = if (f.getName.endsWith("bz"))
      new CompressorStreamFactory().createCompressorInputStream(bis)
    else bis
    val inputSource = scala.io.Source.fromInputStream(stream, encoding) //UTF-8")
    val text = inputSource.getLines mkString "\n"
    inputSource.close()
    text
  }

  /**
   * export a string to file
   * @param outputFile file location as string
   * @param exportString string to export
   * @param append append to existing file - default false
   * @param compress compress in bzip2 - default false
   */
  def exportStringToFile(outputFile :String, exportString: String, append : Boolean = false, compress : Boolean = false): Unit = {
    val outputStream = new FileOutputStream(outputFile, append)
    val printWriter = new PrintWriter(if (compress) new BZip2CompressorOutputStream(outputStream) else outputStream)
    printWriter.write(exportString + "\n")
    printWriter.close()
  }
}

