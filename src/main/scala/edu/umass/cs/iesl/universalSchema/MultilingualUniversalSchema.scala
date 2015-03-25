package edu.umass.cs.iesl.universalSchema

import cc.factorie.epistemodb._

import scala.util.Random

/**
 * Created by beroth on 2/23/15.
 */

class TrainTestMultilingualUniversalSchemaOptions extends cc.factorie.util.DefaultCmdOptions {
  val extractedData = new CmdOption("training-data", "", "FILE", "tab separated file with training data extracted from text of the form : e1 e2 rel score")
  val freebaseData = new CmdOption("freebase-data", "", "FILE", "tab separated file with freebase data of the form : e1 e2 rel score")
  val dim = new CmdOption("dim", 100, "INT", "dimensionality of data")
  val testCount = new CmdOption("test-count", 1000, "INT", "number of cells to test")
  val stepsize = new CmdOption("stepsize", 0.1, "DOUBLE", "step size")
  val maxNorm =  new CmdOption("max-norm", 1.0, "DOUBLE", "maximum l2-norm for vectors")
  val useMaxNorm =  new CmdOption("use-max-norm", true, "BOOLEAN", "whether to use maximum l2-norm for vectors")
  val prune =  new CmdOption("prune", false, "BOOLEAN", "Prune non largest connected component")
  val regularizer = new CmdOption("regularizer", 0.01, "DOUBLE", "regularizer")
  val encoding = new CmdOption("encoding", "UTF-8", "String", "encoding of input data")
}


object TrainTestMultilingualUniversalSchema {

  val opts = new TrainTestMultilingualUniversalSchemaOptions

  def main(args: Array[String]) : Unit = {
    opts.parse(args)

    val tReadStart = System.currentTimeMillis
    var (kb, testCols) = MultilingualEntityRelationKBMatrix.fromTsv(
      opts.extractedData.value, opts.freebaseData.value, encoding = opts.encoding.value, colsPerEnt = 1)
    if (opts.prune.wasInvoked) kb = kb.prune(2,1)

    val tRead = (System.currentTimeMillis - tReadStart)/1000.0
    println(f"Reading from file and pruning took $tRead%.2f s")

    println("Stats:")
    println("Num Rows:" + kb.numRows())
    println("Num Cols:" + kb.numCols())
    println("Num cells:" + kb.nnz())
    println("Test Cols:" + testCols.size)

    val random = new Random(0)
    val numDev = 0
    val numTest = opts.testCount.value
    val (trainKb, devKb, testKb) = kb.randomTestSplit(numDev, numTest, None, Some(testCols), random)

    val model = UniversalSchemaModel.randomModel(kb.numRows(), kb.numCols(), opts.dim.value, random)

    val trainer = if(opts.useMaxNorm.value) {
      println("use norm constraint")
      new NormConstrainedBprUniversalSchemaTrainer(opts.maxNorm.value, opts.stepsize.value, opts.dim.value,
        trainKb.matrix, model, random)
    } else {
      println("use regularization")
      new RegularizedBprUniversalSchemaTrainer(opts.regularizer.value, opts.stepsize.value, opts.dim.value,
        trainKb.matrix, model, random)
    }
    var result = model.similaritiesAndLabels(trainKb.matrix, testKb.matrix)
    println("Initial MAP: " + Evaluator.meanAveragePrecision(result))

    trainer.train(10)

    result = model.similaritiesAndLabels(trainKb.matrix, testKb.matrix)
    println("MAP after 10 iterations: " + Evaluator.meanAveragePrecision(result))

    trainer.train(40)

    result = model.similaritiesAndLabels(trainKb.matrix, testKb.matrix)
    println("MAP after 50 iterations: " + Evaluator.meanAveragePrecision(result))

    trainer.train(50)

    result = model.similaritiesAndLabels(trainKb.matrix, testKb.matrix)
    println("MAP after 100 iterations: " + Evaluator.meanAveragePrecision(result))

    trainer.train(100)

    result = model.similaritiesAndLabels(trainKb.matrix, testKb.matrix)
    println("MAP after 200 iterations: " + Evaluator.meanAveragePrecision(result))
  }

}

object MultilingualEntityRelationKBMatrix {

  private def entitiesAndRelFromLine(line: String, colsPerEnt:Int): (EntityPair, String, Double) = {
    val parts = line.split("\t")
    val e1 : String = parts.slice(0, colsPerEnt).mkString("\t")
    val e2 : String = parts.slice(colsPerEnt, 2 * colsPerEnt).mkString("\t")
    val rel : String = parts.slice(2 * colsPerEnt, parts.length - 1).mkString("\t")
    val cellVal : Double = parts(parts.length - 1).toDouble
    (EntityPair(e1, e2), rel, cellVal)
  }
  // Loads a matrix from a tab-separated file
  def fromTsv(filename:String, freebaseFile:String, encoding : String = "UTF-8", colsPerEnt:Int = 2)
  : (EntityRelationKBMatrix, Set[String]) = {
    val kb = new EntityRelationKBMatrix()
    scala.io.Source.fromFile(filename, encoding).getLines().foreach(line => {
      if (line != "") {
        val (ep, rel, cellVal) = entitiesAndRelFromLine(line, colsPerEnt)
        kb.set(ep, rel, cellVal)
      }
    })
    // get freebase relations as text columns
    val testCols = Set[String]()
    scala.io.Source.fromFile(freebaseFile, encoding).getLines().foreach(line => {
      if (line != "") {
        val (ep, rel, cellVal) = entitiesAndRelFromLine(line, colsPerEnt)
        kb.set(ep, rel, cellVal)
        testCols.+(rel)
      }
    })
    (kb, testCols)
  }
}