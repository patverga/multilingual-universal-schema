package edu.umass.cs.iesl.knowledgeBase

import java.io.PrintWriter

import cc.factorie.util.CmdOptions

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by pv on 3/10/15.
 */
object FreebaseRelationsFromMentions
{
  def main (args : Array[String]) {
    val opts = new FreebaseProcessingOpts
    opts.parse(args)
    assert(opts.inputFileName.wasInvoked, "Must supply mention relation file.")
    assert(opts.outputFileName.wasInvoked, "Must supply output file location.")

//    exportFreebaseRelations(opts.inputFileName.value, opts.outputFileName.value, opts.freebaseFileName.value)
    exportDBPediaRelations(opts.inputFileName.value, opts.outputFileName.value)
  }

  def exportFreebaseRelations(inputFile:  String, outputFile : String, encoding : String = "ISO-8859-1",
                              freebaseFile : String = "/iesl/canvas/pat/data/freebase/freebase-two-entities.formated"): Unit = {

    print(s"Exporting freebase relations for mentions in $inputFile... ")
    // mention pairings we saw in the text
    val mentionPairs = new mutable.HashMap[String, Set[String]]
    // relations from freebase containing valid mention pairings
    val relations = new mutable.HashMap[String, ArrayBuffer[(String, String)]]

    // read in input mentions extracted from text
    val mentionSource = scala.io.Source.fromFile(inputFile, encoding) //UTF-8")
    mentionSource.getLines().foreach(line => {
      val tuple = line.split("\t")
      if (tuple.length > 3) {
        val arg1 = tuple(0)
        val arg2 = if (tuple.length == 6) tuple(2) else tuple(1)
        mentionPairs.put(arg1, mentionPairs.getOrElse(arg1, Set[String]()) + arg2)
      }
    })
    mentionSource.close()
    //  mentionPairs.foreach{case(arg1, args) =>
    //    args.foreach(arg2 => println(arg1, arg2))
    //  }

    // read through freebase dump extracting relations
    val freebaseSource = scala.io.Source.fromFile(freebaseFile) //UTF-8")
    freebaseSource.getLines().foreach(line => {
      //    val (arg1, rel, arg2) = line.split("\t")
      val tuple = line.split("\t")
      val arg1 = tuple(0)
      val rel = tuple(1)
      val arg2 = tuple(2)
      if (mentionPairs.contains(arg1)) {
        val arg2Set = mentionPairs.getOrElse(arg1, Set())
        if (arg2Set.contains(arg2)) {
          //        println(s"$arg1\t$arg2")
          relations.put(rel, relations.getOrElse(rel, ArrayBuffer[(String, String)]()) += ((arg1, arg2)))
        }
      }
    })
    freebaseSource.close()

    val printWriter = new PrintWriter(outputFile)
    relations.foreach { case (rel, args) =>
      args.foreach { case (arg1, arg2) =>
        printWriter.println(s"$arg1\t$arg2\t$rel\t1.0")
      }
    }
    printWriter.close()
    println("Done")
  }

  def exportDBPediaRelations(inputFile:  String, outputFile : String, encoding : String = "ISO-8859-1"): Unit = {

    print(s"Exporting freebase relations for mentions in $inputFile... ")
    val prefix = "<http://rdf.freebase.com/ns/"
    val printWriter = new PrintWriter(outputFile)

    // read in input mentions extracted from text
    val mentionSource = scala.io.Source.fromFile(inputFile, encoding) //UTF-8")
    mentionSource.getLines().foreach(line => {
      val tuple = line.split("\t")
      if (tuple.length > 3) {
        val dbId1 = FreebaseWikiBiMap.freebase2DBPedia(s"$prefix${tuple(0).replaceAll("/",".")}>")
        val dbId2 = FreebaseWikiBiMap.freebase2DBPedia(s"$prefix${(if (tuple.length == 6) tuple(2) else tuple(1)).replaceAll("/",".")}>")
        if (dbId1.isDefined && dbId1.isDefined && dbId1 != dbId1) {
          val arg1 = dbId1.get
          val arg2 = dbId2.get
          for (i <- 1 to 3) {
            val query = Virtuoso.constructAllPathsQuery(arg1, arg2, maxHops = i, freebase = false)
            val paths = Virtuoso.runQuery(query)
//            println(query)
//            if (paths.nonEmpty) {
              val pathStrings = paths.mkString(s"$arg1\t$arg2\t", s"\t1.0\n$arg1\t$arg2\t", "\t1.0")
              println(pathStrings)
              printWriter.println(pathStrings)
//            }
          }
        }
      }
    })
    mentionSource.close()

    printWriter.close()
    println("Done")
  }
}



class FreebaseProcessingOpts extends CmdOptions{
  val inputFileName = new CmdOption[String]("relation-filename", "", "FILENAME", "File containing preprocessed entity relation tuples in Universal schema tsv format.")
  val freebaseFileName = new CmdOption[String]("freebase-filename", "/iesl/canvas/pat/data/freebase/freebase-two-entities.formated", "FILENAME", "File of preprocessed freebase dump in tsv form (fid1 relation fid2)")
  val outputFileName = new CmdOption[String]("output-filename", "", "FILENAME", "File to output extracted freebase relations from.")
}