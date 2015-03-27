package edu.umass.cs.iesl.knowledgeBase

import java.io.File

import scala.io.Source

/**
* Created by pv on 3/13/15.
*/

class FreebaseWikiBiMap(f2wFile:File, f2dbFile:File) {

  val s1 = Source.fromFile(f2wFile)
  val f2w = s1.getLines().map { line =>
    val Array(fId, wId, _) = line.split("\t")
    (FreebaseId(fId), WikipediaId(wId))
  }.toMap
  s1.close()

  val w2f = f2w.map(_.swap)

  val s2 = Source.fromFile(f2dbFile)
  val f2db = s2.getLines().map { line =>
    val Array(dbId, _, fId) = line.split(" ")
    (fId, dbId)
  }.toMap
  s2.close()

  val db2f = f2db.map(_.swap)

  def apply(fId:FreebaseId):Option[(WikipediaId)] = f2w.get(fId)

  def apply(wId:WikipediaId):Option[(FreebaseId)] = w2f.get(wId)

//  def apply(dbId:DBPediaId):Option[(FreebaseId)] = db2f.get(dbId)

  def freebase2DBPedia(fId:String):Option[(String)] = f2db.get(fId)
}
object FreebaseWikiBiMap extends FreebaseWikiBiMap(
  new File("/iesl/canvas/beroth/tac/data/freebase_to_wiki_types.tsv"),
  new File("/home/pat/data/dbpedia/freebase_links.nt"))


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
case class DBPediaId(value:String) extends IdForm
case class SurfaceFormId(value:String) extends IdForm

