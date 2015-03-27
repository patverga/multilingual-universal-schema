package edu.umass.cs.iesl.knowledgeBase

import com.hp.hpl.jena.query.{QueryExecutionFactory, QueryFactory}

import scala.collection.mutable.ArrayBuffer


object Virtuoso
{

  val endpoint = "http://dbpedia.org/sparql"
  val queryPrefix =
    "PREFIX dbpedia-owl: <http://dbpedia.org/ontology/>\n" +
      "PREFIX dbpprop: <http://dbpedia.org/property/>\n" +
      "PREFIX dbres: <http://dbpedia.org/resource/>\n" +
      "PREFIX sameAs: <http://www.w3.org/2002/07/owl#sameAs>\n"

  /**
   * pointless test method
   */
  def checkServiceTest() {
    val query = "ASK { }";
    val qe = QueryExecutionFactory.sparqlService(endpoint, query)
    try {
      if (qe.execAsk()) {
        System.out.println(endpoint + " is UP");
      } // end if
    } catch {
      case e: Exception =>
        System.out.println(endpoint + " is DOWN");
    } finally {
      qe.close()
    }
  }

  /**
   * execute a given  SPARQL query
   * @param queryString a valid SPARQL query
   * @return a formated resultset
   */
  def runQuery(queryString:String): Seq[String] ={
    val query = QueryFactory.create(queryString)
    val qexec = QueryExecutionFactory.sparqlService(endpoint, query)
    val results = qexec.execSelect()
    val resultStrings = new ArrayBuffer[String]()
    while(results.hasNext) {
      val path = new StringBuilder()
      val nextResult = results.next()
      val vars = nextResult.varNames()
      while(vars.hasNext) {
        path.append(nextResult.get(vars.next()).toString)
        if (vars.hasNext) path.append("__")
      }
      resultStrings += path.toString()
    }
//    val out = ResultSetFormatter.asText(results, query)
    //    out
    qexec.close()
    resultStrings.toSeq
  }

  /**
   * Constructs a virtuoso sparql query given a starting entity and a path of relations
   * @param startEntity starting entity
   * @param relations path of relations to take
   * @return constructed query
   */
  def constructKnownPathQuery(startEntity : String, relations : Seq[String]): String =
  {
    val query =
      s"SELECT ?x${relations.size-1} WHERE {" +
        relations.zipWithIndex.map{ case(rel, i) =>
          if(i == 0) s"dbres:$startEntity dbpprop:${relations.head} ?x0 ."
          else s"?x${i-1} dbpprop:$rel ?x$i ."
        }.mkString("\n", "\n", "\n") +
        "}"

    queryPrefix + query
  }

  /**
   * Constructs a virtuoso sparql query given a starting entity and a path of relations
   * @param start starting entity
   * @param end ending entity
   * @param maxHops maximum length of path between the start and end entities
   * @return constructed query
   */
  def constructAllPathsQuery(start : String, end : String, maxHops : Int = 2, freebase:Boolean = false, printEntities:Boolean = false): String =
  {
    val query = new StringBuilder(queryPrefix)
    // select the entity and relation from each hop
    query.append(s"\nSELECT DISTINCT \n${(for (i <- 0 to maxHops-1; line = if (i < maxHops-1 && printEntities)s"?h$i ?x$i" else s"?h$i") yield line).toList.mkString(" ")} WHERE {")
    // simple 1 hop query
    if (maxHops == 1)
      query.append(s"\n$start ?h0 $end .\n")
    else
    {
      // create the query for the actual hops
      query.append((for (i <- 0 to maxHops-1;
                         line = if(i == 0 && freebase) s"?x0 ?h0 $start ."
                           else if(i == 0 && !freebase) s"$start ?h0 ?x0 ."
                         else if(i == maxHops-1) s"?x${i-1} ?h$i $end ."
                         else s"?x${i-1} ?h$i ?x$i .") yield line
        ).mkString("\n", "\n", "\n")
      )
      //filter against cyles
      query.append("\nFILTER(\n")
      for (i <- 0 to maxHops - 2) {
        if (i > 0) query.append(" &&\n")
        query.append(s"?h$i != sameAs: &&\n")
        query.append(s"?x$i != $start &&\n?x$i != $end")
        for (j <- i + 1 to maxHops - 2) {
          query.append(s" &&\n?x$i != ?x$j")
        }
      }
      query.append(s"\n&& ?h${maxHops-1} != sameAs:\n")
      query.append(")\n")
    }
    query.append("}")
    query.toString()
  }

  def stringToDBPediaResource(input : String) : String ={
    s"dbres:$input"
  }

  def stringToFreebase(input : String) : String ={
    s"<http://rdf.freebase.com/ns/$input>"
  }

  def dbPediaToFreebase(dbPediaID : String): String ={
    val query = "SELECT DISTINCT ?x0 WHERE { \n" +
      s"dbres:$dbPediaID sameAs: ?x0 .\n" +
      "FILTER(STRSTARTS(STR(?x0), \"http://rdf.freebase.com/ns/\"))\n"
    queryPrefix + query + "}"
  }

  def main(args : Array[String]): Unit ={
////     get grand children of barack obamas dad
//      val query1 = constructKnownPathQuery("Lolo_Soetoro", Seq("children", "children"))
//      println(query1)
//      val results1 = runQuery(query1)
//      println(results1.mkString("\n"))

//     2 hop relations between barack obama and his dad
        val query2 = constructAllPathsQuery(stringToDBPediaResource("Lolo_Soetoro"), stringToDBPediaResource("Barack_Obama"), maxHops = 3)
        println(query2)
        val results2 = runQuery(query2)
        println(results2.mkString(s"A\tB\t", s"\t1.0\nA\tB\t", "\t1.0"))

//    val query3 = dbPediaToFreebase("Amsterdam")
//    println(query3)
//    val results3 = runQuery(query3)
//    println(results3.mkString("\n"))
  }
}