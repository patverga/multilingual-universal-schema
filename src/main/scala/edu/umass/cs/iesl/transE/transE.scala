package edu.umass.cs.iesl.transE

import java.io.{File, FileInputStream}
import java.util
import java.util.zip.GZIPInputStream

import cc.factorie.app.nlp.embeddings._
import cc.factorie.la.{Tensor, DenseTensor1, WeightsMapAccumulator}
import cc.factorie.optimize._
import cc.factorie.util.{CmdOptions, Threading, DoubleAccumulator}
import cc.factorie.model.{Parameters, Weights}

import scala.collection.mutable
import scala.collection.mutable.{PriorityQueue, ArrayBuffer}
import scala.util.Random

/**
 * Created by pat on 4/3/15.
 */
class TransE(val opts: TransEOpts) extends Parameters {

  val D = 50
  var weights: Seq[Weights] = null
  val gamma = 1

  protected val threads = 1
  protected val adaGradDelta = 0
  protected val adaGradRate = 0.01
  protected val encoding = "UTF-8"

  protected val minRelationCount = 1
  protected val negativeSamples = 1
  protected val iterations = 10000
  protected val miniBatchSize = 1000

  protected var trainer: Trainer = null
  protected var optimizer: GradientOptimizer = null

  protected val relationVocab = new util.HashMap[String, Int]
  protected val entityVocab = new util.HashMap[String, Int]
  var relationCount = 0
  var entityCount = 0

  val rand = new Random(69)


  def buildVocab(inFile: String): Seq[(String, String, String)] = {
    println("Building Vocab")
    val corpusLineItr = inFile.endsWith(".gz") match {
      case true => io.Source.fromInputStream(new GZIPInputStream(new FileInputStream(inFile)), encoding).getLines()
      case false => io.Source.fromInputStream(new FileInputStream(inFile), encoding).getLines()
    }
    val relationMap = new mutable.HashMap[String, ArrayBuffer[(String, String, String)]]
    while (corpusLineItr.hasNext) {
      val line = corpusLineItr.next()
      //      val (e1, relation, e2) = parseArvind(line)
      val (e1, relation, e2) = parseTsv(line)
      if (!entityVocab.containsKey(e1)) {
        entityVocab.put(e1, entityCount)
        entityCount += 1
      }
      if (!entityVocab.containsKey(e2)) {
        entityVocab.put(e2, entityCount)
        entityCount += 1
      }
      if (!relationVocab.containsKey(relation)) {
        relationVocab.put(relation, relationCount)
        relationCount += 1
      }
      relationMap.put(relation, relationMap.getOrElse(relation, new ArrayBuffer()) += ((e1, relation, e2)))
    }
    // flatten input triplets
    relationMap.filter(eList => eList._2.size >= minRelationCount).toSeq.flatMap(eList => eList._2.toSet.toSeq)
  }

  // assumes arvind format : [e1,e2\trelation\tscore]
  def parseArvind(line: String): (String, String, String) = {
    val Array(entities, relation, score) = line.split("\t")
    val Array(e1, e2) = entities.split(",")
    (e1, relation, e2)
  }

  def parseTsv(line: String): (String, String, String) = {
    val parts = line.split("\t")
    (parts(0), parts(1), parts(2))
  }

  // Component-2
  def learnEmbeddings(relationList: Seq[(String, String, String)]): Unit = {
    println("Learning Embeddings")

//    optimizer = new AdaGradRDA(delta = adaGradDelta, rate = adaGradRate)
    optimizer = new AdaGrad(delta = adaGradDelta, rate = adaGradRate)
    trainer = new HogwildTrainer(weightsSet = this.parameters, optimizer = optimizer, nThreads = threads, maxIterations = Int.MaxValue)
//    trainer = new OnlineTrainer(weightsSet = this.parameters, optimizer = optimizer, maxIterations = Int.MaxValue)

//    weights = (0 until entityCount + relationCount).map(i => Weights(TensorUtils.setToRandom1(new DenseTensor1(D, 0)))) // initialized using wordvec random
    //TODO make sure this works
    weights = Seq.fill(entityCount + relationCount)(Weights(TensorUtils.setToRandom1(new DenseTensor1(D, 0)))) // initialized using wordvec random
    optimizer.initializeWeights(this.parameters)

    // normalize relation embeddings once
    //TODO check bounds
    (entityCount until relationCount + entityCount).par.foreach(weights(_).value.twoNormalize())
    println(weights.size, entityCount, entityVocab.size(), relationCount, relationVocab.size())

    for (iteration <- 0 to iterations) {
      println(s"Training iteration: $iteration")
      // sample miniBatch
      val miniBatch = Seq.fill(miniBatchSize)(relationList(rand.nextInt(relationList.size)))
      // normalize entity embeddings
      // TODO check bounds
//      for (i <- 0 to entityCount -1) weights(i).value.twoNormalize()
      (0 until entityCount).par.foreach(weights(_).value.twoNormalize())

      processMiniBatch(miniBatch)
    }
    println("Done learning embeddings. ")
    //store()
  }

  def evaluate(relationList: Seq[(String, String, String)]): Unit = {
    val headRanks = new ArrayBuffer[Int]
    val tailRanks = new ArrayBuffer[Int]
    var tripletIndex = 0
    // rank each triplet in the set
    while (tripletIndex < relationList.size) {
      val (e1, relation, e2) = relationList(tripletIndex)
      val e1Id = entityVocab.get(e1)
      val e2Id = entityVocab.get(e2)
      val e1Emb = weights(e1Id).value
      val e2Emb = weights(e2Id).value
      val relEmb = weights(relationVocab.get(relation) + entityCount).value

      var headRank = 0
      var tailRank = 0
      val score = (e1Emb + relEmb - e2Emb).twoNorm

      var negativeId = 0
      while (negativeId < entityVocab.size()) {
        val negEmb = weights(negativeId).value
        // dont self rank
        if(negativeId != e2Id && negativeId != e2Id) {
          val negHeadScore = (e1Emb + relEmb - negEmb).twoNorm
          if (negHeadScore < score)
            headRank += 1
          val negTailScore = (negEmb + relEmb - e2Emb).twoNorm
          if (negTailScore < score)
            tailRank += 1
        }
        negativeId += 1
      }
      if (tripletIndex % 1000 == 0) println(tripletIndex.toDouble/relationList.size.toDouble, tailRank, headRank)
      tailRanks += tailRank
      headRanks += headRank
      tripletIndex += 1
    }
    val ranks = tailRanks ++ headRanks
    println(ranks.count(_ <= 10).toDouble / (relationList.size * 2).toDouble, ranks.sum / ranks.length)
  }


  protected def processMiniBatch(relationList: Seq[(String, String, String)]): Unit = {
    val examples = relationList.map{case (e1, relation, e2) =>
      val e1Index = entityVocab.get(e1)
      val e2Index = entityVocab.get(e2)
      val relationIndex = relationVocab.get(relation) + entityCount
      // corrupt either head or tail
      if (rand.nextInt(2) == 0) new TansEExample(this, e1Index, relationIndex, e2Index, rand.nextInt(entityCount), e2Index)
      else new TansEExample(this, e1Index, relationIndex, e2Index, e1Index, rand.nextInt(entityCount))
    }
     trainer.processExamples(examples)
  }
}

object TestTransE extends App {

  val opts = new TransEOpts()
  opts.parse(args)

  val transE = new TransE(opts)
//  val train = transE.buildVocab("/Users/pat/universal-schema/transe/SME/data/FB15k/freebase_mtr100_mte100-train.txt")
//  val test = transE.buildVocab("/Users/pat/universal-schema/transe/SME/data/FB15k/freebase_mtr100_mte100-test.txt")
  val train = transE.buildVocab(opts.train.value)
  val test = transE.buildVocab(opts.test.value)
  println(train.size, test.size)
  transE.learnEmbeddings(train)
  transE.evaluate(test)

}

class TransEOpts extends CmdOptions{
  val train = new CmdOption[String]("train", "", "FILENAME", "Train file.")
  val test = new CmdOption[String]("test", "", "FILENAME", "Test File.")
  val outputFileName = new CmdOption[String]("output-filename", "", "FILENAME", "Output")
}


class TansEExample(model: TransE, e1PosDex: Int, relDex: Int, e2PosDex: Int, e1NegDex: Int, e2NegDex: Int, l1: Boolean = false) extends Example {

  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit =
  {
    val e1PosEmb = model.weights(e1PosDex).value
    val e1NegEmb = model.weights(e1NegDex).value

    val e2PosEmb = model.weights(e2PosDex).value
    val e2NegEmb = model.weights(e2NegDex).value

    val relEmb = model.weights(relDex).value

    var posGrad = e2PosEmb - e1PosEmb - relEmb
    var negGrad = e2NegEmb - e1NegEmb - relEmb
//    posGrad = if (l1 && posGrad > 0) 1 else if (l1 && posGrad <= 0) -1 else posGrad

    // gamma + pos - neg
    val objective = if (l1) model.gamma + posGrad.oneNorm - negGrad.oneNorm
    else model.gamma + posGrad.twoNorm - negGrad.twoNorm

    val factor: Double = 1.0

    if (value ne null) value.accumulate(objective)

    // hinge loss
    if (gradient != null && objective > 0.0)
    {
      gradient.accumulate(model.weights(e1PosDex), posGrad, factor)
      gradient.accumulate(model.weights(e2PosDex), posGrad, -factor)
      gradient.accumulate(model.weights(relDex), posGrad, factor)
      gradient.accumulate(model.weights(e1NegDex), negGrad, -factor)
      gradient.accumulate(model.weights(e2NegDex), negGrad, factor)
      gradient.accumulate(model.weights(relDex), negGrad, -factor)
//      gradient.accumulate(model.weights(relDex), e1PosEmb+e2NegEmb-e2PosEmb-e1NegEmb, factor)

    }
  }
}
