package edu.umass.cs.iesl.knowledgeBase

import java.io.PrintWriter

import cc.factorie.util.CmdOptions

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by pv on 3/10/15.
 */
object FreebaseRelationsFromMentions extends App
{
  val opts = new FreebaseProcessingOpts
  opts.parse(args)
  assert(opts.inputFileName.wasInvoked, "Must supply mention relation file.")
  assert(opts.freebaseFileName.wasInvoked, "Must supply processed freebase-dump.rdf file.")
  assert(opts.outputFileName.wasInvoked, "Must supply output file location.")

  // mention pairings we saw in the text
  val mentionPairs = new mutable.HashMap[String, Set[String]]
  // relations from freebase containing valid mention pairings
  val relations = new mutable.HashMap[String, ArrayBuffer[(String, String)]]

  // read in input mentions extracted from text
  val mentionSource = scala.io.Source.fromFile(opts.inputFileName.value, "ISO-8859-1") //UTF-8")
  mentionSource.getLines().foreach(line => {
    val tuple = line.split("\t")
    if (tuple.length > 3 ){
      val arg1 = tuple(0)
      val arg2 = tuple(2)
      mentionPairs.put(arg1, mentionPairs.getOrElse(arg1, Set[String]()) + arg2)
    }
  })
  mentionSource.close()
//  mentionPairs.foreach{case(arg1, args) =>
//    args.foreach(arg2 => println(arg1, arg2))
//  }

  // read through freebase dump extracting relations
  val freebaseSource = scala.io.Source.fromFile(opts.freebaseFileName.value) //UTF-8")
  freebaseSource.getLines().foreach(line => {
//    val (arg1, rel, arg2) = line.split("\t")
    val tuple = line.split("\t")
    val arg1 = tuple(0)
    val rel = tuple(1)
    val arg2 = tuple(2)
    if (mentionPairs.contains(arg1)){
      val arg2Set = mentionPairs.getOrElse(arg1, Set())
      if (arg2Set.contains(arg2)){
//        println(s"$arg1\t$arg2")
        relations.put(rel, relations.getOrElse(rel, ArrayBuffer[(String, String)]()) += ((arg1, arg2)))
      }
    }
  })
  freebaseSource.close()

  val printWriter = new PrintWriter(opts.outputFileName.value)
  relations.foreach{case(rel, args) =>
    args.foreach{case(arg1, arg2) =>
      printWriter.println(s"$arg1\t$arg1\t$arg2\t$arg2\t$rel\t1.0")
    }
  }
  printWriter.close()
}

class FreebaseProcessingOpts extends CmdOptions{
  val inputFileName = new CmdOption[String]("relation-filename", "inputFileName", "FILENAME", "File containing preprocessed entity relation tuples in Universal schema tsv format.")
  val freebaseFileName = new CmdOption[String]("freebase-filename", "freebaseFileName", "FILENAME", "File of preprocessed freebase dump in tsv form (fid1 relation fid2)")
  val outputFileName = new CmdOption[String]("output-filename", "outputFileName", "FILENAME", "File to output extracted freebase relations from.")
}