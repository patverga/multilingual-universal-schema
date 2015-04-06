package edu.umass.cs.iesl.transE

import java.io.{File, FileInputStream}
import java.util
import java.util.zip.GZIPInputStream

import cc.factorie.app.nlp.embeddings._
import cc.factorie.la.{DenseTensor1, WeightsMapAccumulator}
import cc.factorie.optimize.{AdaGradRDA, Example}
import cc.factorie.util.{Threading, DoubleAccumulator}
import cc.factorie.model.{ Parameters, Weights }

import scala.collection.mutable
import scala.collection.mutable.{PriorityQueue, ArrayBuffer}

/**
 * Created by pat on 4/3/15.
 */
class TransE(val opts: EmbeddingOpts) extends Parameters{

  // Algo related
  val D = 50 //opts.dimension.value
  // default value is 200
  var V: Int = 0
  // vocab size. Will computed in buildVocab() section
  protected val threads = opts.threads.value
  //  default value is 12
  protected val adaGradDelta = 0.01 //opts.delta.value
  // default value is 0.1
  protected val adaGradRate = opts.rate.value
  //  default value is 0.025
  protected val minCount = opts.minCount.value
  // default value is 5
  protected val ignoreStopWords = if (opts.ignoreStopWords.value) 1 else 0
  // default value is 0
  protected val vocabHashSize = opts.vocabHashSize.value
  // default value is 20 M. load factor is 0.7. So, Vocab size = 0.7 * 20M = 14M vocab supported which is sufficient enough for large amounts of data
  protected val samplingTableSize = opts.samplingTableSize.value
  // default value is 100 M
  protected val maxVocabSize = opts.vocabSize.value

  val gamma = 1
  val minRelationCount = 1 //10
  val negativeSamples = 1
  val iterations = 1

  // IO Related
  // corpus input filename. Code takes cares of .gz extension
  protected val outputFilename = opts.output.value
  // embeddings output filename
  private val storeInBinary = if (opts.binary.value) 1 else 0
  // binary=1 will make both vocab file (optional) and embeddings in .gz file
  private val loadVocabFilename = opts.loadVocabFile.value
  // load the vocab file. Very useful for large corpus should you run multiple times
  private val saveVocabFilename = opts.saveVocabFile.value
  // save the vocab into a file. Next time for the same corpus, load it . Saves lot of time on large corpus
  private val encoding = opts.encoding.value
  // Default is UTF8
  // data structures
  protected var trainer: LiteHogwildTrainer = null
  // modified version of factorie's hogwild trainer for speed by removing logging and other unimportant things. Expose processExample() instead of processExamples()
  protected var optimizer: AdaGradRDA = null

  var relationWeights: Seq[Weights] = null
  var entityWeights: Seq[Weights] = null
  // EMBEDDINGS . Will be initialized in learnEmbeddings() after buildVocab() is called first


//  val relationSet = mutable.HashSet[String]()
  val relationVocab = new VocabBuilder()
//  val entitySet = mutable.HashSet[String]()
  val entityVocab = new VocabBuilder()



  // assumes arvind format : [e1,e2\trelation\tscore]
  def buildVocab(inFile : String): Seq[(String, String, String)] =
  {
    println("Building Vocab")
    val corpusLineItr = inFile.endsWith(".gz") match
    {
      case true => io.Source.fromInputStream(new GZIPInputStream(new FileInputStream(inFile)), encoding).getLines()
      case false => io.Source.fromInputStream(new FileInputStream(inFile), encoding).getLines()
    }
    val relationMap = new mutable.HashMap[String, ArrayBuffer[(String, String, String)]]
    while (corpusLineItr.hasNext) {
      val line = corpusLineItr.next()
//      val (e1, relation, e2) = parseArvind(line)
      val (e1, relation, e2) = parseTsv(line)
      relationVocab.addWordToVocab(relation)
      entityVocab.addWordToVocab(e1)
      entityVocab.addWordToVocab(e2)
      relationMap.put(relation, relationMap.getOrElse(relation, new ArrayBuffer()) += ((e1, relation, e2)))
    }
    entityVocab.buildSamplingTable() // for getting random word from vocab in O(1) otherwise would O(log |V|)
    // flatten input triplets
    relationMap.filter(eList => eList._2.size >= minRelationCount).toSeq.flatMap(eList => eList._2.toSet.toSeq)
  }

  def parseArvind(line: String): (String, String, String) ={
    val Array(entities, relation, score) = line.split("\t")
    val Array(e1, e2) = entities.split(",")
    (e1, relation, e2)
  }

  def parseTsv(line: String): (String, String, String) ={
    val parts = line.split("\t")
    (parts(0), parts(1), parts(2))
  }

  // Component-2
  def learnEmbeddings(relationList : Seq[(String, String, String)]): Unit = {
    println("Learning Embeddings")
    optimizer = new AdaGradRDA(delta = adaGradDelta, rate = adaGradRate)
    entityWeights = (0 until entityVocab.size()).map(i => Weights(TensorUtils.setToRandom1(new DenseTensor1(D, 0)))) // initialized using wordvec random
    relationWeights = (0 until relationVocab.size()).map(i => Weights(TensorUtils.setToRandom1(new DenseTensor1(D, 0)))) // initialized using wordvec random
    optimizer.initializeWeights(this.parameters)
    trainer = new LiteHogwildTrainer(weightsSet = this.parameters, optimizer = optimizer, nThreads = threads, maxIterations = Int.MaxValue)
    val threadIds = (0 until threads).map(i => i)
    val sliceSize = relationList.size / threads
    for (iteration <- 0 to iterations)
      Threading.parForeach(threadIds, threads)(threadId => workerThread(threadId, relationList.slice(threadId*sliceSize, (1+threadId)*sliceSize)))
    println("Done learning embeddings. ")
    //store()
  }

  def rankLearnedEmbeddings(relationList : Seq[(String, String, String)]): Unit =
  {
    val headRanks = new ArrayBuffer[Int]
    val tailRanks = new ArrayBuffer[Int]
    var triplet = 0
    // rank each triplet in the set
    while (triplet < relationList.size){
      val (e1, relation, e2) = relationList(triplet)
      var negative = 0
      val e1Id = entityVocab.getId(e1)
      val e2Id = entityVocab.getId(e2)
      val e1Emb = entityWeights(e1Id).value
      val e2Emb = entityWeights(e2Id).value
      val relEmb = relationWeights(relationVocab.getId(relation)).value

      var headRank = 0
      var tailRank = 0
      val score = tripletScore(e1Emb, relEmb, e2Emb)
      while (negative < entityVocab.size())
      {
        val negEmb = entityWeights(negative).value
        // dont self rank
        if (negative != e2Id && tripletScore(e1Emb, relEmb, negEmb) < score)
            headRank += 1

        if (negative != e1Id && tripletScore(negEmb, relEmb, e2Emb) < score)
            tailRank += 1

        negative += 1
      }
      println(tailRank, headRank)
      tailRanks += tailRank
      headRanks += headRank
      triplet += 1
    }
  }

  def tripletScore(e1 : Weights#Value, rel :Weights#Value, e2: Weights#Value): Double ={
    1.0 - (e1 + rel).l2Similarity(e2)
  }

  def process(e1: String, relation : String, e2 : String): Int =
  {
    val e1Index = entityVocab.getId(e1)
    val e2Index = entityVocab.getId(e2)
    val relationIndex = relationVocab.getId(relation)
    // make the examples
    trainer.processExample(new TansEExample(this, e1Index, relationIndex, e2Index, entityVocab.getRandWordId(), true))
    trainer.processExample(new TansEExample(this, e1Index, relationIndex, e2Index, entityVocab.getRandWordId(), false))
    1
  }

  protected def workerThread(id: Int, relationList : Seq[(String, String, String)], printAfterNDoc: Long = 100): Unit = {
    var word_count: Long = 0
    var i = 0
    while (i < relationList.size) {
      val (e1, relation, e2) = relationList(i)
      word_count += process(e1, relation, e2) // Design choice : should word count be computed here and just expose process(doc : String): Unit ?.
//      if (id == 1 && i % printAfterNDoc == 0) { // print the process after processing 100 docs in 1st thread. It approx reflects the total progress
//        println("Progress : " + word_count / relationList.size.toDouble * 100 + " %")
//      }
      i += 1
    }
  }
}

object TestTransE extends App {

  val transE = new TransE(new EmbeddingOpts)
  val train = transE.buildVocab("/Users/pat/universal-schema/transe/Relation_Extraction/data/nips13-dataset/Freebase/train.txt")
  val test = transE.buildVocab("/Users/pat/universal-schema/transe/Relation_Extraction/data/nips13-dataset/Freebase/test.txt")
  println(train.size, test.size)
  transE.learnEmbeddings(train)
  transE.rankLearnedEmbeddings(test)

}


class TansEExample(model: TransE, e1 : Int, relation : Int, e2 : Int, corruptIndex :Int, corruptTail: Boolean) extends Example {

  // to understand the gradient and objective refer to : http://arxiv.org/pdf/1310.4546.pdf
  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator): Unit =
  {
    val e1Embedding = model.entityWeights(e1).value
    val e2Embedding = model.entityWeights(e2).value
    val relationEmbedding = model.relationWeights(relation).value
    val corruptEmbedding = model.entityWeights(corruptIndex).value

    // d(e1 + relation, e2)
    val posScore = model.tripletScore(e1Embedding, relationEmbedding, e2Embedding)
    // d(e1 + relation, corrupt) or d(corrupt + relation, e2)
    val negScore = if (corruptTail) model.tripletScore(e1Embedding, relationEmbedding, corruptEmbedding)
      else model.tripletScore(corruptEmbedding, relationEmbedding, e2Embedding)
    // gamma + pos - neg
    val objective = model.gamma + posScore - negScore
//    println(objective)
    val factor: Double = 1.0

    if (value ne null) value.accumulate(objective)
    if (gradient ne null) {

      if (!corruptTail)gradient.accumulate(model.entityWeights(e1), e1Embedding, factor)
      else gradient.accumulate(model.entityWeights(e2), e2Embedding, factor)
      gradient.accumulate(model.relationWeights(relation), relationEmbedding, factor)
      gradient.accumulate(model.entityWeights(corruptIndex), corruptEmbedding, -factor)
    }

  }
}
