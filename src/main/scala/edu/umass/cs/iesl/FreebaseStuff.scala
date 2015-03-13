package edu.umass.cs.iesl

import java.io.{PrintWriter, File}

import cc.factorie.util.{CmdOptions, DefaultCmdOptions, CmdOption}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Created by pv on 3/10/15.
 */
class FreebaseWikiBiMap(f2wFile:File) {

  val f2w = Source.fromFile(f2wFile).getLines().map { line =>
    val Array(fId, wId, _) = line.split("\t")
    (FreebaseId(fId), WikipediaId(wId))
  }.toMap

  val w2f = f2w.map(_.swap)

  def apply(fId:FreebaseId):Option[(WikipediaId)] = f2w.get(fId)

  def apply(wId:WikipediaId):Option[(FreebaseId)] = w2f.get(wId)
}
object FreebaseWikiBiMap extends FreebaseWikiBiMap(new File("/iesl/canvas/beroth/tac/data/freebase_to_wiki_types.tsv"))


sealed trait IdForm {
  def value:String
  def freebaseOption = this match {
    case fId:FreebaseId => Some(fId)
    case _ => None
  }
}

object IdForm {
  val FreebaseIdRegex = """(/m[/\.]\w{5,})""".r // todo check that this is correct
  def fromString(rawVal:String):IdForm = rawVal match {
      case FreebaseIdRegex(mid) => FreebaseId(mid)
      case surfaceForm => SurfaceFormId(surfaceForm)
    }

}

case class WikipediaId(value:String) extends IdForm
case class FreebaseId(value:String) extends IdForm
case class SurfaceFormId(value:String) extends IdForm

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
  val mentionSource = scala.io.Source.fromFile(opts.inputFileName.value) //UTF-8")
  mentionSource.getLines().foreach(line => {
    val tuple = line.split("\t")
    val arg1 = tuple(0)
    val arg2 = tuple(1)
    mentionPairs.put(arg1, mentionPairs.getOrElse(arg1, Set[String]()) + arg2)
  })
  mentionSource.close()
  //  mentionPairs.foreach{case(arg1, args) =>
  //    args.foreach(arg2 => println(arg1, arg2))
  //    }

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
        relations.put(rel, relations.getOrElse(rel, ArrayBuffer[(String, String)]()) += ((arg1, arg2)))
      }
    }
  })
  freebaseSource.close()

  val printWriter = new PrintWriter(opts.outputFileName.value)
  relations.foreach{case(rel, args) =>
    args.foreach{case(arg1, arg2) =>
      printWriter.println(s"$arg1\t$arg1\t$rel")
    }
  }
  printWriter.close()
}

class FreebaseProcessingOpts extends CmdOptions{
  val inputFileName = new CmdOption[String]("relation-filename", "inputFileName", "FILENAME", "File containing preprocessed entity relation tuples in Universal schema tsv format.")
  val freebaseFileName = new CmdOption[String]("freebase-filename", "freebaseFileName", "FILENAME", "File of preprocessed freebase dump in tsv form (fid1 relation fid2)")
  val outputFileName = new CmdOption[String]("output-filename", "outputFileName", "FILENAME", "File to output extracted freebase relations from.")
}