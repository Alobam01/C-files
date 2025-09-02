//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Standalone Scalation Simulation:
 *  - Customers arrive with feature vectors (x)
 *  - Each customer has a target value (y)
 *  - A regression model is updated incrementally
 *  - Results are collected in a Sink
 *
 *  Works with Scala 2.12 + Scalation process library
 */
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.simulation.process._
import scalation.random._
import scalation.mathstat._
import scalation.modeling._
import scala.collection.mutable.ArrayBuffer

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Define a Customer entity with feature vector x and response y
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

class Customer(val x: VectorD, val y: Double, val model: Regression, val results: ArrayBuffer[Double])
  extends SimActor("c", null) {

  def act(): Unit = {
    // Update regression model with new observation
    model.addData(x, y)

    // Predict current y
    val yp = model.predict(x)
    results += yp

    // Debug info
    println(s"t = $clock: Customer arrives with x = $x, y = $y, predicted = $yp")
  }
}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Main Simulation Model
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

class RegressionSimulation(name: String, nFeatures: Int, nObs: Int)
  extends Model(name) {

  private val muI = 5.0              // mean interarrival time
  private val iArrivalRV = Exponential(muI)
  private val model = new Regression(null, null)   // regression model
  private val results = ArrayBuffer[Double]()      // store predictions

  // Source generates Customers
  val source = Source("CustomerArrivals", this, () => {
    // Random feature vector
    val x = VectorD.rand(nFeatures)
    // True response with some noise
    val y = 3.0 + 2.0 * x.sum + Normal(0, 0.5).gen
    new Customer(x, y, model, results)
  }, iArrivalRV, nObs, jTime = 0.0)

  val sink = Sink("Finished", this)

  // Hook source -> sink
  addComponent(source, sink)
}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Run the Simulation
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

@main def runRegressionSimulation(): Unit = {
  val sim = new RegressionSimulation("RegressionSim", nFeatures = 3, nObs = 10)
  sim.simulate()
  sim.waitFinished()
  Model.shutdown()
}
