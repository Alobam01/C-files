//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Optimizer_SGD_Standalone.scala
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.linalgebra._
import scalation.util._
import scala.math.min

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Hyperparameters for training
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
case class HyperParameter(
                           bSize: Int = 5,        // batch size
                           maxEpochs: Int = 50,   // maximum epochs
                           upLimit: Int = 1000    // unused placeholder for stop criteria
                         )

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Neural network parameter container
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
case class NetParam(w: MatrixD, b: VectorD)
type NetParams = Array[NetParam]
type AFF = (MatrixD => MatrixD, MatrixD => MatrixD)  // (activation, derivative)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// SGD Optimizer class
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class Optimizer_SGD(hp: HyperParameter) {

  // Simple ReLU activation and derivative
  val relu: MatrixD => MatrixD = _.map(x => if (x > 0) x else 0.0)
  val reluPrime: MatrixD => MatrixD = _.map(x => if (x > 0) 1.0 else 0.0)

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Optimize a 2-layer network
  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def optimize2(x: MatrixD, y: MatrixD, b: NetParams, eta: Double, ff: Array[AFF]): (Double, Int) = {

    val permGen = (0 until x.dim).toArray  // simple permutation vector
    val actFun = ff(0)
    val batchSize = min(hp.bSize, x.dim)
    val maxEpochs = hp.maxEpochs
    var sseBest = Double.MaxValue

    for (epoch <- 1 to maxEpochs) {
      // iterate batches
      for (start <- 0 until x.dim by batchSize) {
        val end = min(start + batchSize, x.dim)
        val batchX = x(start until end, ::).toMatrixD
        val batchY = y(start until end, ::).toMatrixD

        // SGD weight update
        val z = batchX * b(0).w + batchY * 0.0  // forward
        val grad = ((b(0).w * batchX) - batchY).t * batchX  // gradient
        b(0).w -= grad * eta
      }

      // compute sum of squared errors
      val pred = x * b(0).w
      val sse = ((pred - y) :^ 2.0).sum
      if (sse < sseBest) sseBest = sse

      println(s"Epoch $epoch: SSE = $sse")
    }

    (sseBest, maxEpochs)
  }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Main program: Run the SGD simulation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
@main def runSGD(): Unit = {

  val hp = HyperParameter()
  val opt = new Optimizer_SGD(hp)

  // Dummy dataset: XOR-like problem
  val x = MatrixD((Array(
    Array(0.0, 0.0),
    Array(0.0, 1.0),
    Array(1.0, 0.0),
    Array(1.0, 1.0)
  )))
  val y = MatrixD((Array(
    Array(0.0),
    Array(1.0),
    Array(1.0),
    Array(0.0)
  )))

  // Initialize parameters (weights & biases)
  val b = Array(NetParam(MatrixD.rand(2,1), VectorD.rand(1)))
  val act = Array((opt.relu, opt.reluPrime))

  println("Running standalone SGD simulation...")
  val (sse, epochs) = opt.optimize2(x, y, b, 0.01, act)
  println(s"Training complete. Best SSE: $sse after $epochs epochs")
}
