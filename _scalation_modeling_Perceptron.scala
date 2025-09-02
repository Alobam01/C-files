//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller / adapted
 *  @version 2.0
 *  @note    Standalone Scalation Simulation: Perceptron (2-layer NN)
 */

import scalation.linalgebra._
import scalation.modeling._
import scalation.util._
import scalation.plot._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Standalone Perceptron class (single-output)
 */
class Perceptron(x: MatrixD, y: VectorD,
                 eta: Double = 0.1, maxEpochs: Int = 400,
                 f: AFF = f_sigmoid) {

  private val m = x.dim
  private val n = x.dim2
  private var b = VectorD.one(n)      // initialize weights with ones

  // Train the perceptron with gradient descent
  def train(): Unit = {
    var sse0 = Double.MaxValue
    var epoch = 1
    var go = true

    while (go && epoch <= maxEpochs) {
      val yp = f.f_(x * b)                  // predicted output
      val e  = y - yp                        // error
      val δ  = -f.d(yp) * e                  // delta
      b -= x.t * δ * eta                     // weight update
      val sse = (y - f.f_(x * b)).normSq
      println(s"Epoch $epoch: b = $b, SSE = $sse")
      if (sse >= sse0) go = false
      else sse0 = sse
      epoch += 1
    }
  }

  // Predict for a new input vector
  def predict(z: VectorD): Double = f.f(b dot z)

  // Predict for a new input matrix
  def predict(z: MatrixD): VectorD = f.f_(z * b)

}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Main function to run the simulation
 */
@main def runPerceptronSimulation(): Unit = {

  println("==== Perceptron Standalone Simulation ====")

  // Toy dataset: 9 points with 3 inputs + bias
  val x = MatrixD((9, 4),
    1.0, 0.0, 0.0, 0.5,
    1.0, 0.0, 0.5, 0.3,
    1.0, 0.0, 1.0, 0.2,
    1.0, 0.5, 0.0, 0.8,
    1.0, 0.5, 0.5, 0.5,
    1.0, 0.5, 1.0, 0.3,
    1.0, 1.0, 0.0, 1.0,
    1.0, 1.0, 0.5, 0.8,
    1.0, 1.0, 1.0, 0.5
  )
  val y = VectorD(0.1, 0.2, 0.1, 0.8, 0.5, 0.3, 1.0, 0.8, 0.5)

  // Create perceptron
  val nn = new Perceptron(x, y, eta = 0.5, maxEpochs = 10)

  // Train perceptron
  nn.train()

  // Predict on training data
  val yp = nn.predict(x)
  println(s"Predicted outputs: $yp")
  println(s"Actual outputs   : $y")
}
