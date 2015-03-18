package edu.umass.cs.iesl.oldtaccode

import java.io.BufferedWriter
import edu.umass.cs.iesl.oldtaccode.Freebase.Relation

import scala.io.Source
import scala.collection.mutable
import cc.factorie.app.nlp.Document


/**
 * @author John Sullivan
 */
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

case class WikipediaId(value:String)

case class FreebaseId(value:String) extends IdForm
case class SurfaceFormId(value:String) extends IdForm

//case class EntityType(tacValue:String, freebaseValues:Iterable[String])

//freebase is left, tac is right
class EntityType(slot:Either[String, String]) {
  def tacValue = slot match {
    case Left(freebase) => EntityType.freebase2Tac(freebase)
    case Right(tac) => tac
  }

  def freebaseValues = slot match {
    case Left(freebase) => EntityType.tac2Freebase(EntityType.freebase2Tac(freebase))
    case Right(tac) => EntityType.tac2Freebase(tac)
  }

  override def toString = "EntityType(%s, %s)".format(tacValue, freebaseValues)
}

object EntityType {
  private val rows = Source.fromInputStream(getClass.getResourceAsStream("/freebase_types_to_ne")).getLines()
    .collect{ case line if !line.startsWith("#") =>
    val Array(freebaseType, tacType) = line.split("\t")
    freebaseType -> tacType
  }.toSeq

  def unapply(et:EntityType):Option[(String, Seq[String])] = Some(et.tacValue -> et.freebaseValues)

  private val tac2Freebase = rows.groupBy(_._2).mapValues(_.map(_._1))
  private val freebase2Tac = rows.toMap

  private val expansions = Map("PER" -> "PERSON",
    "ORG" -> "ORGANIZATION")
  //Map.empty[String, String]

  def fromTacString(rawStr:String):Option[EntityType] = {
    //println("rawStr:" + rawStr)
    val tacStr = expansions.getOrElse(rawStr, rawStr).toUpperCase
    println("tacStr:" + tac2Freebase)
    val r = tac2Freebase.get(tacStr).map(_ => new EntityType(Right(tacStr)))
    //    println("tac2str " + r)
    r
  }

  def fromFreebaseString(freeStr:String):Option[EntityType] = {
    freebase2Tac.get(freeStr).map(_ => new EntityType(Left(freeStr)))
  }

  def main(args:Array[String]) {
    println(tac2Freebase)

    println(freebase2Tac)

    println(EntityType.fromTacString("PERSON"))
  }
}
case class SurfaceRow(arg1:IdForm, arg1Type:EntityType, arg2:IdForm, arg2Type:EntityType, surfaceForm:String, count:Int) {
  def updateArgs(pair:QueryPair):SurfaceRow = updateArgs(pair.arg1.freebaseOption, pair.arg2.freebaseOption)
  def updateArgs(n1:Option[FreebaseId], n2:Option[FreebaseId]):SurfaceRow = n1 -> n2 match {
    case (Some(nArg1), Some(nArg2)) => SurfaceRow(nArg1, arg1Type, nArg2, arg2Type, surfaceForm, count)
    case (Some(nArg1), None) => SurfaceRow(nArg1, arg1Type, arg2, arg2Type, surfaceForm, count)
    case (None, _) => this //if arg1 (ie. the query) is not linked there is nothing we can do
  }

  def writeToFile(wrt:BufferedWriter):SurfaceRow = {
    wrt.write(this.toTSV)
    wrt.newLine()
    wrt.flush()
    this
  }

  def argPair = QueryPair(arg1, arg1Type, arg2, arg2Type) //(arg1 -> arg1Type, arg2 -> arg2Type)

  def toTSV = Array(arg1.value, arg1Type.tacValue, arg2.value, arg2Type.tacValue, surfaceForm, count.toString).mkString("\t")
}

object SurfaceRow {
  def fromTSV(tsvString:String):SurfaceRow = {
    println(tsvString.split("\t").toSeq)
    val Array(arg1, arg1Type, arg2, arg2Type, surfaceForm, count) = tsvString.split("\t")
    SurfaceRow(IdForm.fromString(arg1), EntityType.fromTacString(arg1Type).get, IdForm.fromString(arg2), EntityType.fromTacString(arg2Type).get, surfaceForm, count.toInt)
  }
}
case class QueryPair(arg1:IdForm, arg1Type:EntityType, arg2:IdForm, arg2Type:EntityType) {
  def updateArg1(newArg:IdForm):QueryPair = QueryPair(newArg, arg1Type, arg2, arg2Type)
  def updateArg2(newArg:IdForm):QueryPair = QueryPair(arg1, arg1Type, newArg, arg2Type)

  def buildTSVString(rel:Relation):String = "%s\t%s\t%s\t%s\t%s\t%s".format(arg1.value, arg1Type.tacValue, arg2.value, arg2Type.tacValue, rel.tacName, 1)
}

object QueryPair {
  def fromSurfaceRow(row:SurfaceRow):Option[QueryPair] = row match {
    case SurfaceRow(arg1 @ FreebaseId(_), arg1Type, arg2, arg2Type, _, _) => Some(QueryPair( arg1, arg1Type, arg2, arg2Type))
    case _ => None
  }
}

case class Offsets(arg1Span:(Int, Int), arg2Span:(Int, Int), totalSpan:(Int, Int))

object Offsets {
  def fromString(str:String):Offsets = {
    val Array(a, b ,c) = str.split(":")
    val Array(arg1Start, arg1End) = a.split("-")
    val Array(arg2Start, arg2End) = b.split("-")
    val Array(totalStart, totalEnd) = c.split("-")
    Offsets(arg1Start.toInt -> arg1End.toInt, arg2Start.toInt -> arg2End.toInt, totalStart.toInt -> totalEnd.toInt)
  }
}

case class Context(arg1:IdForm, arg1Type:EntityType, arg2:IdForm, arg2Type:EntityType, docId:FileId, offsets:Offsets, surfaceForm:String) {
  def retrieveDocument = Context.docMap(docId)
}

object Context {
  var tacRoot = "" //todo populate this
  protected val docMap = mutable.HashMap[FileId, Document]().withDefault(docId => SerializePlainText.deserializeFromFileId(new Document(), docId, tacRoot))

  def fromTSV(tsvString:String):Context = {
    val Array(arg1, arg1Type, arg2, arg2Type, docId, offsets, surfaceForm) = tsvString.split("\t")
    Context(IdForm.fromString(arg1), EntityType.fromTacString(arg1Type).get, IdForm.fromString(arg2), EntityType.fromTacString(arg2Type).get, FileId(docId), Offsets.fromString(offsets), surfaceForm)
  }
}
