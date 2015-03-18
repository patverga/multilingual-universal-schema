package edu.umass.cs.iesl.oldtaccode

import java.io.File

case class FileId(service: String, id1: String, id2: String) {
  val dfPattern = "(bolt.*)".r
  val webPattern = "(eng.*)".r
  def asNormalizedId: String = "%s-%s-%s".format(service, id1, id2)
  def asId: String = service match{
    case dfPattern(_) => s"$service-${id1.take(3)}-${id1.drop(3)}-$id2"
    case webPattern(_) => s"$service-${id1.take(2)}-${id1.drop(2)}-$id2"
    case _ => s"${service}_$id1.$id2"
  }
  // for backwards compatibility
  def asNormalizedFilepath(root: String, extension: String): File = new File(asNormalizedFilepathStr(root, extension))
  def asNormalizedFilepathStr(root: String, extension: String): String = "%s/%s/%s/%s.%s".format(root, service, id1, asNormalizedId, extension)

  def asFilepath(root: String, extension: String): File = new File(asFilepathStr(root, extension))
  def asFilepathStr(root: String, extension: String): String = "%s/%s/%s/%s.%s".format(root, service, id1, asId, extension)
  def makeDirectory(root: String): Boolean = new File("%s/%s/%s".format(root, service, id1)).mkdirs()
}

object FileId {
  //  private val DocRegex = """([\w_]+)_(\d{8})_(.+)""".r
  private val DocRegex = """(\D+)[-_](\d+-?\d+)[-.](\d+)""".r

  // e.g. eng-NG-31-109501-8131900, bolt-eng-DF-170-181103-15978491
  //  private val DocRegex1 = """\s+-(\d+)-(\d{4})(\d{2})-(\d+)""".r

  // e.g. AFP_ENG_20091101.0001
  //  private val DocRegex2 = """\s+_(\d{4})(\d{2})(\d{2})\.(\d+)""".r

  def apply(file: File): FileId = {
    //    println(s"file.getName: ${file.getName}")
    //    println(s"file.getName: ${file.getName.split(".").mkString("|")}")
    val idString = file.getName.dropRight(4) //.replaceFirst("[.][^.]+$", "").replaceAll("""\.""","_")
    val DocRegex(source, id1, id2) = idString
    new FileId(source, id1.replaceFirst("-", ""), id2)
  }

  def apply(idString: String): FileId = {
    val processedIdString = idString.trim.replaceFirst("\\.\\D+", "") // get rid of possible file extension
    val DocRegex(source, id1, id2) = processedIdString
    new FileId(source, id1.replaceFirst("-", ""), id2)
  }
}
