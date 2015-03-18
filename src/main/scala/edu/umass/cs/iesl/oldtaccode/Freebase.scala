package edu.umass.cs.iesl.oldtaccode

import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

import scala.io.Source
//import dispatch._
//import Defaults._
import org.json4s._
/**
 * @author John Sullivan
 */
object Freebase {
  implicit def null2JValue(n:Null):JValue = JNull

  def idQueryByName(form:String, fTypes:Iterable[String]):String = compact(render(
    Seq(("mid" -> null) ~
      ("name" -> form) ~
      ("a:type|=" -> fTypes) ~
      ("b:type" -> ("return" -> "count")) ~
      ("optional" -> true))))

  case class Path(targetType:String, value:IndexedSeq[String])
  case class Relation(tacName:String, tacType:String, sourceType:String, paths:Iterable[Path]) {
    def toQueryPart(pair:QueryPair) = pair match {
      case QueryPair(FreebaseId(arg1Id), EntityType(_, arg1Types), FreebaseId(arg2Id), EntityType(_, arg2TypesSeq))
        if arg1Types.toSet.contains(sourceType) =>
        val arg2Types = arg2TypesSeq.toSet
        paths.flatMap {
          case path if arg2Types.contains(path.targetType) => Some(path.value.foldRight[JObject](("mid", arg2Id) ~ ("optional", true)){
            case (pathElem, jObj) => pathElem -> Seq(jObj)
          })
          case otw => None
        }
      case otw => Seq.empty[JObject] //todo consider other options
    }

    def existsIn(json:JValue):Boolean = paths.exists{ path =>
      path.value.foldLeft(json)(_ \ _) \ "mid" != JNothing
    }
  }

  object Relation {
    private val relMap = Source.fromInputStream(getClass.getResourceAsStream("/FreebaseRelations")).getLines()
      .filterNot(line => line.startsWith("#") || line.trim == "")
      .map{ line =>
      val tacName :: sourceType :: rest = line.split("""\s+""").toList
      val tacType = tacName.split(":").head.toUpperCase
      val targetType = rest.last
      val pathValue = rest.take(rest.size - 1)
      (tacType, tacName, sourceType) -> Path(targetType, pathValue.toIndexedSeq)
    }.toList.groupBy(_._1._2).mapValues{ vs =>
      val (tacType, tacName, sourceType) = vs.head._1
      val paths = vs.map(_._2)
      Relation(tacName, tacType, sourceType, paths)
    }.values.groupBy(_.tacType)

    relMap.foreach{ case (k, v) =>
      println(k -> v)
    }

    def getForType(argType:String):Iterable[Relation] = relMap(argType)
    def extractFoundRelations(resp:JValue):Iterable[Relation] = relMap.values.flatten.filter(_.existsIn(resp))
  }


  def relationQueryByIds(pair:QueryPair):String = {
    println(pair)
    println(Relation.getForType(pair.arg1Type.tacValue))

    pretty(render(
      ("mid" -> pair.arg1.value) ~
        ("type|=" -> pair.arg2Type.freebaseValues) ~
        Relation.getForType(pair.arg1Type.tacValue).flatMap(_.toQueryPart(pair)).reduce(_ ~ _)))
  }


//  def main(args:Array[String]) {
//    val q = QueryPair(FreebaseId("/m/02mjmr"), EntityType.fromTacString("PER").get, FreebaseId("/m/025s5v9"), EntityType.fromTacString("PER").get)
//    println(q)
//    println(relationQueryByIds(q))
//    val key = "AIzaSyC-vqylUeqdMY8OOgN2KZGU3b2NxgkWlMc"
//    val baseURL = "https://www.googleapis.com/freebase/v1/mqlread"
//
//    val exec = new FreebaseExecutor(key, baseURL, Relation.extractFoundRelations )
//    println("sending request")
//    val res = exec(relationQueryByIds(q))
//
//    for(r <- res) {
//      println("response received")
//      println(r)
//    }
//    println("done")
//  }
}

//class FreebaseExecutor[Result](apiKey:String, baseUrl:String, process:(JValue => Result)) {
//
//  def apply(queryString:String):Future[Result] =
//    Http(url(baseUrl)
//      <<? Map("key" -> apiKey, "query" -> queryString)
//      > as.json4s.Json).map(json => process(json \ "result"))
//}