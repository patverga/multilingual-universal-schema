package edu.umass.cs.iesl

import java.io.File

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