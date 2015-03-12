package edu.umass.cs.iesl

import java.io.File
import java.util

import scala.collection.mutable
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

//object FreebaseRelationsFromMentions extends App {
//  assert(args.length > 1, "must supply mention relation file and freebase-dump.rdf file.")
//  // mention pairings we saw in the text
//  val mentionPairs = new mutable.HashMap[String, Set[String]]
//  // relations from freebase containing valid mention pairings
//  val relations = new mutable.HashMap[String, Seq[(String, String)]]
//
//  // read in input mentions extracted from text
//  val mentionSource = scala.io.Source.fromFile(args(0)) //UTF-8")
//  mentionSource.getLines().foreach(line => {
//    val (arg1, arg2, _, _) = line.split("\t")
//    // TODO current freebase dump has entity urls not just id
//    mentionPairs.put(arg1, mentionPairs.getOrElse(arg1, Set[String]()) + arg2)
//  })
//  mentionSource.close()
//
//  // read through freebase dump extracting relations
//  val freebaseSource = scala.io.Source.fromFile(args(1)) //UTF-8")
//  freebaseSource.getLines().foreach(line => {
//    val tuple = line.split("\t")
//  })
//  freebaseSource.close()
//}