//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// AdamOptimizerSimulation.scala
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.linalgebra._
import scalation.random._
import scalation.util._
import scala.math._

//=================== Dummy types for simplification ===========================
case class NetParam(w: MatrixD, b: VectorD) {
  def copy = NetParam(w.copy, b.copy)
  def set(other: NetParam): Unit = { w := other.w; b := other.b }
  def -=(other: NetParam): Unit = { w :-= other.w; b :-= other.b }
}
type NetParams = Array[NetParam]

// Dummy activation function family
trait AFF {
  def fM(x: MatrixD): MatrixD = x map (v => 1.0 / (1.0 + exp(-v)))
  def dM(x: MatrixD): MatrixD = x map (v => v * (1.0 - v))
}

// Hyper-parameters
object hp {
  val params = collection.mutable.Map[String, Double](
    "bSize" -> 5, "maxEpochs" -> 50, "upLimit" -> 10, "beta" -> 0.9, "beta2" -> 0.999
  )
  def apply(key: String) = params(key)
}

//=================== Optimizer_Adam ===========================
class Optimizer_Adam {

  val EPSILON = 1E-8

  def optimize(x: MatrixD, y: MatrixD,
               b: NetParams, eta_: Double, f: Array[AFF]): (Double, Int) = {

    val bSize = hp("bSize").toInt
    val maxEpochs = hp("maxEpochs").toInt
    val β1 = hp("beta")
    val β2 = hp("beta2")

    var p = b.map(bp => new MatrixD(bp.w.dim, bp.w.dim2))  // first moment
    var v = b.map(bp => new MatrixD(bp.w.dim, bp.w.dim2))  // second moment
    var p_bias = b.map(bp => new VectorD(bp.b.dim))
    var v_bias = b.map(bp => new VectorD(bp.b.dim))

    var eta = eta_
    var sse_best = Double.MaxValue
    var epoch = 0

    while (epoch < maxEpochs) {
      var sse = 0.0
      for (i <- 0 until x.dim) {
        val xi = x(i, ?)
        val yi = y(i, ?)
        // Simple feedforward
        val ai = xi
        val y_hat = f(0).fM(ai * b(0).w) + b(0).b
        val err = y_hat - yi
        sse += (err.t * err)(0)

        // Adam update
        p(0) := β1 * p(0) + (1 - β1) * (err.t * xi)
        v(0) := β2 * v(0) + (1 - β2) * ((err.t * xi) ~^ 2)
        val m_hat = p(0) / (1 - β1 ~^ (epoch + 1))
        val v_hat = v(0) / (1 - β2 ~^ (epoch + 1))
        b(0).w :-= (eta * m_hat /:/ (v_hat.mapValues(vv => sqrt(vv) + EPSILON)))
      }
      if (sse < sse_best) sse_best = sse
      epoch += 1
    }
    (sse_best, epoch)
  }
}

//=================== Main Simulation ===========================
@main def AdamOptimizerSimulation(): Unit = {
  println("=== Adam Optimizer Standalone Simulation ===")

  // Generate dummy dataset: y = 2*x + noise
  val m = 20   // number of samples
  val n = 2    // number of features
  val X = new MatrixD(m, n, (i, j) => Uniform(0, 1).draw())
  val Y = X * VectorD(2.0, -1.0).toDenseMatrix + new MatrixD(m, 1, (i,j)=>Gaussian(0,0.1).draw())

  // Initialize network parameters
  val W = new MatrixD(n, 1, 0.1)    // weights
  val bVec = new VectorD(1, 0.0)    // bias
  val netParams: NetParams = Array(NetParam(W, bVec))

  val optimizer = new Optimizer_Adam
  val (loss, epochs) = optimizer.optimize(X, Y, netParams, 0.05, Array(new AFF {}))

  println(s"Final loss: $loss after $epochs epochs")
  println(s"Updated weights: ${netParams(0).w}")
  println(s"Updated bias: ${netParams(0).b}")
}
