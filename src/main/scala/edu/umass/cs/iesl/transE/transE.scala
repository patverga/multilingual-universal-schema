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

  val D = 100
  var weights: Seq[Weights] = null
  val gamma = 1.0
  // use L1 distance, L2 otherwise
  val l1 = true

  protected val threads = 4
  protected val adaGradDelta = 0.0
  protected val adaGradRate = 0.01
  protected val encoding = "UTF-8"

  protected val minRelationCount = 1
  protected val negativeSamples = 1
  protected val iterations = 1000
  protected val numBatches = 100

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
  def learnEmbeddings(trainTriplets: Seq[(String, String, String)]): Unit = {
    println("Learning Embeddings")
    val batchSize = trainTriplets.size / numBatches

    //    optimizer = new AdaGradRDA(delta = adaGradDelta, rate = adaGradRate)
    optimizer = new AdaGrad(delta = adaGradDelta, rate = adaGradRate)
//    trainer = new HogwildTrainer(weightsSet = this.parameters, optimizer = optimizer, nThreads = threads, maxIterations = Int.MaxValue)
    trainer = new OnlineTrainer(weightsSet = this.parameters, optimizer = optimizer, maxIterations = Int.MaxValue, logEveryN = batchSize)

    weights = (0 until entityCount + relationCount).map(i => Weights(TensorUtils.setToRandom1(new DenseTensor1(D, 0)))) // initialized using wordvec random
    optimizer.initializeWeights(this.parameters)

    // normalize relation embeddings once
    (entityCount until weights.size).par.foreach(weights(_).value.twoNormalize())
    println(weights.size, entityCount, entityVocab.size(), relationCount, relationVocab.size())

    for (iteration <- 0 to iterations) {
      println(s"Training iteration: $iteration")
      // normalize entity embeddings
//      (0 until entityCount).par.foreach(weights(_).value.twoNormalize())
      // normalize relations : paper says they dont do this but makes results better
      (0 until entityCount + relationCount).par.foreach(weights(_).value.twoNormalize())

      (0 until numBatches).foreach(batch => {
        // sample random triplets for miniBatch
        val miniBatch = Seq.fill(batchSize)(trainTriplets(rand.nextInt(trainTriplets.size)))
        processMiniBatch(miniBatch)
      })
    }
    println("Done learning embeddings. ")
    //store()
  }

  protected def processMiniBatch(relationList: Seq[(String, String, String)]): Unit = {
    val examples = relationList.map { case (e1, relation, e2) =>
      val e1Index = entityVocab.get(e1)
      val e2Index = entityVocab.get(e2)
      val relationIndex = relationVocab.get(relation) + entityCount
      // corrupt either head or tail
      if (rand.nextInt(2) == 0) new TansEExample(this, e1Index, relationIndex, e2Index, rand.nextInt(entityCount), e2Index, l1)
      else new TansEExample(this, e1Index, relationIndex, e2Index, e1Index, rand.nextInt(entityCount), l1)
    }
    trainer.processExamples(examples)
  }

  /**
   * for each test triplet, rank the correct answer amongst all corrupted head triplets
   * and all corrupted tail triplets
   * @param testTriplets test triples in form e1 relation e2
   * @return (hits@10, averageRank)
   */
  def evaluate(testTriplets: Seq[(String, String, String)]): (Double, Double) = {

    println(s"Evaluating on ${testTriplets.size} samples")
    val ranks: Seq[Int] = testTriplets.par.flatMap { case (e1, relation, e2) =>
      val e1Id = entityVocab.get(e1)
      val e2Id = entityVocab.get(e2)
      val e1Emb = weights(e1Id).value
      val e2Emb = weights(e2Id).value
      val relEmb = weights(relationVocab.get(relation) + entityCount).value

      var headRank = 0
      var tailRank = 0
      val posScore = if (l1) (e1Emb + relEmb - e2Emb).oneNorm else (e1Emb + relEmb - e2Emb).twoNorm

      // store to save time
      val e1Rel = e1Emb + relEmb
      val relE2 = relEmb - e2Emb

      // iterate over each other entity in dictionary
      var negativeId = 0
      while (negativeId < entityCount) {
        // dont self rank
        if (negativeId != e1Id && negativeId != e2Id) {
          val negEmb = weights(negativeId).value
          val negHeadScore = if (l1) (e1Rel - negEmb).oneNorm else (e1Rel - negEmb).twoNorm
          if (negHeadScore < posScore)
            headRank += 1
          val negTailScore = if (l1) (negEmb + relE2).oneNorm else (negEmb + relE2).twoNorm
          if (negTailScore < posScore)
            tailRank += 1
        }
        negativeId += 1
      }
      Seq(headRank, tailRank)
    }.seq
    // return hits@10 and avg rank
    (ranks.count(_ < 10).toDouble / ranks.size.toDouble, ranks.sum / ranks.length)
  }
}

class TansEExample(model: TransE, e1PosDex: Int, relDex: Int, e2PosDex: Int, e1NegDex: Int, e2NegDex: Int, l1: Boolean = false) extends Example {

  val factor: Double = 1.0

  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit = {
    val e1PosEmb = model.weights(e1PosDex).value
    val e1NegEmb = model.weights(e1NegDex).value

    val e2PosEmb = model.weights(e2PosDex).value
    val e2NegEmb = model.weights(e2NegDex).value

    val relEmb = model.weights(relDex).value

    var posGrad = e2PosEmb - e1PosEmb - relEmb
    var negGrad = e2NegEmb - e1NegEmb - relEmb

    // gamma + pos - neg
    val objective = if (l1) model.gamma + posGrad.oneNorm - negGrad.oneNorm
    else model.gamma + posGrad.twoNorm - negGrad.twoNorm

    if (l1) {
      (0 until posGrad.size).foreach(i => {
        posGrad(i) = if (posGrad(i) > 0) 1.0 else -1.0
        negGrad(i) = if (negGrad(i) > 0) 1.0 else -1.0
      })
    }

    if (value ne null) value.accumulate(objective)

    // hinge loss
    if (gradient != null && objective > 0.0) {
      gradient.accumulate(model.weights(e1PosDex), posGrad, factor)
      gradient.accumulate(model.weights(e2PosDex), posGrad, -factor)
      gradient.accumulate(model.weights(relDex), posGrad, factor)
      gradient.accumulate(model.weights(e1NegDex), negGrad, -factor)
      gradient.accumulate(model.weights(e2NegDex), negGrad, factor)
      gradient.accumulate(model.weights(relDex), negGrad, -factor)

    }
  }
}

object TestTransE extends App {

  val opts = new TransEOpts()
  opts.parse(args)

  val transE = new TransE(opts)
  val train = transE.buildVocab(opts.train.value)
  val test = transE.buildVocab(opts.test.value)
  println(train.size, test.size)
  transE.learnEmbeddings(train)
  println(transE.evaluate(test))

}

class TransEOpts extends CmdOptions {
  val train = new CmdOption[String]("train", "", "FILENAME", "Train file.")
  val test = new CmdOption[String]("test", "", "FILENAME", "Test File.")
  val outputFileName = new CmdOption[String]("output-filename", "", "FILENAME", "Output")
}


