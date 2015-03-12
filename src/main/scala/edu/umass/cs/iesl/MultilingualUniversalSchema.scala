package edu.umass.cs.iesl

import scala.util.Random
import cc.factorie.epistemodb._

/**
 * Created by beroth on 2/23/15.
 */

class TrainTestMultilingualUniversalSchemaOptions extends cc.factorie.util.DefaultCmdOptions {
  val tacData = new CmdOption("training-data", "", "FILE", "tab separated file with training data of the form : e1 e2 rel score")
  val dim = new CmdOption("dim", 100, "INT", "dimensionality of data")
  val testCount = new CmdOption("test-count", 1000, "INT", "number of cells to test")
  val stepsize = new CmdOption("stepsize", 0.1, "DOUBLE", "step size")
  val maxNorm =  new CmdOption("max-norm", 1.0, "DOUBLE", "maximum l2-norm for vectors")
  val useMaxNorm =  new CmdOption("use-max-norm", true, "BOOLEAN", "whether to use maximum l2-norm for vectors")
  val prune =  new CmdOption("prune", true, "BOOLEAN", "Prune non largest connected component")
  val regularizer = new CmdOption("regularizer", 0.01, "DOUBLE", "regularizer")
}


object TrainTestMultilingualUniversalSchema {

  val opts = new TrainTestMultilingualUniversalSchemaOptions

//  val testCols = Set("")

  def main(args: Array[String]) : Unit = {
    opts.parse(args)

    val tReadStart = System.currentTimeMillis
    var kb = EntityRelationKBMatrix.fromTsv(opts.tacData.value, colsPerEnt = 2)
    if (opts.prune.wasInvoked) kb = kb.prune(2,1)

    val tRead = (System.currentTimeMillis - tReadStart)/1000.0
    println(f"Reading from file and pruning took $tRead%.2f s")

    println("Stats:")
    println("Num Rows:" + kb.numRows())
    println("Num Cols:" + kb.numCols())
    println("Num cells:" + kb.nnz())

    val random = new Random(0)
    val numDev = 0
    val numTest = opts.testCount.value
    val (trainKb, devKb, testKb) = kb.randomTestSplit(numDev, numTest, None, None, random)

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
