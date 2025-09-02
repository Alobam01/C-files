//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller (adapted to standalone)
 *  @version 2.0
 *  @date    Fri Aug 29 23:15:00 WAT 2025
 *  @note    Standalone Simulation: Monitoring Loss over Epochs
 */

import scalation.linalgebra._
import scalation.plot._
import scala.collection.mutable.ArrayBuffer
import scalation.util.Monitor

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MonitorLoss` trait provides methods to track the convergence of
 *  optimization algorithms based on the value of the loss function.
 */
trait MonitorLoss extends Monitor {

  private val losses = ArrayBuffer[Double]()   // hold values for loss function

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Collect the next value for the loss function.
   *  @param loss  the value of the loss function
   */
  def collectLoss(loss: Double): Unit = {
    losses += loss
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Plot the loss function versus the epoch/major iterations.
   *  @param optName  the name of optimization algorithm (alt. name of network)
   */
  def plotLoss(optName: String): Unit = {
    val lossVec  = VectorD(losses.toArray)
    val epochs   = VectorD.range(1, lossVec.dim + 1)
    new Plot(epochs, lossVec, s"Loss vs Epochs for $optName", true)
  }

} // end MonitorLoss

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Example simulation: fake optimization that reduces loss each epoch.
 */
object MonitorLossSimulation extends App with MonitorLoss {

  println("Starting Loss Monitoring Simulation...")

  // Fake training loop (e.g., gradient descent simulation)
  val maxEpochs = 50
  var currentLoss = 10.0

  for (epoch <- 1 to maxEpochs) {
    // simulate loss decay
    currentLoss = currentLoss * 0.95 + scala.util.Random.nextGaussian() * 0.1
    collectLoss(currentLoss)
    println(f"Epoch $epoch%2d => Loss = $currentLoss%2.4f")
  }

  // Plot the loss vs epoch curve
  plotLoss("FakeOptimizer")

}
