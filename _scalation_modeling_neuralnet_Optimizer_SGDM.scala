//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// OptimizerSGDMSimulation.scala
// Standalone Scalation Simulation for SGD with Momentum
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.linalgebra._
import scalation.util.Timer
import scalation.random._

// Dummy NetParams and AFF classes for this simulation
case class NetParam(var w: MatrixD, var b: MatrixD)
type NetParams = Array[NetParam]
trait AFF {
  def fM(x: MatrixD): MatrixD  // activation function forward
  def dM(x: MatrixD): MatrixD  // activation derivative
}

// Simple Sigmoid Activation
object Sigmoid extends AFF {
  def fM(x: MatrixD): MatrixD = x.map(el => 1.0 / (1.0 + math.exp(-el)))
  def dM(x: MatrixD): MatrixD = {
    val y = fM(x)
    y ⊙ (1.0 - y)
  }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// SGD with Momentum Optimizer
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class Optimizer_SGDM {

  def optimize2(x: MatrixD, y: MatrixD,
                bb: NetParams, eta: Double, ff: Array[AFF]): (Double, Int) = {

    val b = bb(0)
    val f = ff(0)
    val maxEpochs = 100
    val beta = 0.9        // momentum parameter
    val nB = x.dim / 2    // batch size

    var p = new MatrixD(b.w.dim, b.w.dim2)      // momentum matrix
    var epoch = 0
    var sse_best = Double.PositiveInfinity

    while (epoch < maxEpochs) {
      epoch += 1
      // Dummy batch iteration (here we just do full batch for simplicity)
      val α = f.fM(b.w * x)
      val error = α - y
      val grad = x.t * (error ⊙ f.dM(b.w * x))
      p = grad * (1 - beta) + p * beta
      b.w -= grad * eta + p * eta

      val sse = (error ⊙ error).sum
      if (sse < sse_best) sse_best = sse
      println(s"Epoch $epoch, SSE = $sse_best")
    }
    (sse_best, epoch)
  }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Standalone Main Program
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
@main def runSGDMSimulation(): Unit = {
  println("Starting SGD with Momentum Simulation...")

  // Dummy dataset: 4 samples, 2 features
  val x = new MatrixD((4, 2), 0.1, 0.5, 0.2, 0.4, 0.3, 0.9, 0.7, 0.8)
  val y = new MatrixD((4, 1), 0.0, 1.0, 0.0, 1.0)

  // Initialize network parameters (weights & biases)
  val b = NetParam(new MatrixD((1, 2), 0.5, -0.3), new MatrixD((1, 1), 0.0))
  val netParams: NetParams = Array(b)

  // Activation function
  val activations: Array[AFF] = Array(Sigmoid)

  // Instantiate optimizer
  val optimizer = new Optimizer_SGDM

  // Run optimization
  val (sse, epochs) = optimizer.optimize2(x, y, netParams, 0.1, activations)
  println(s"Training complete. Final SSE = $sse after $epochs epochs.")
}
