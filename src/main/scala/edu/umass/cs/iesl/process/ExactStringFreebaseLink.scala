package edu.umass.cs.iesl.process

import cc.factorie.app.nlp.Document
import edu.umass.cs.iesl.entity_embeddings.data_structures.{Slug, EntityLinks}
import edu.umass.cs.iesl.entity_embeddings.linking.EntityLinker

import scala.io.Source

/**
 * Created by pv on 3/20/15.
 */

object ExactStringFreebaseLink extends ExactStringFreebaseLink("/home/pat/data/freebase/english_freebase_names") {

  override def process(document: Document): Document = {
    val mentions = document.attr[EntityLinks].mentions.toArray
    mentions.foreach(mention=>{
      val unsluggedMention = Slug.unSlug(mention.mentionSlug)
      val fbLink = linkName(unsluggedMention)
      mention.entitySlug = if (fbLink != None) s"${fbLink.get}\t$unsluggedMention" else ""
    })
    document
  }
}

abstract class ExactStringFreebaseLink(idNameFile : String) extends EntityLinker
{   
  val idNameMap = Source.fromFile(idNameFile).getLines().map { line =>
    val Array(fId, name) = line.split("\t")
    (name, fId)
  }.toMap

  
  def linkName(name : String) : Option[String] ={
    val fid = if (idNameMap.contains(name)) idNameMap.get(name) else None   
    if (fid != None) println(s"Sucessfully linked $name") else println ("Could not link name")
    fid
  }
}
