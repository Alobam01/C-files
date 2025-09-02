//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Standalone Scalation Simulation: Non-Homogeneous Erlang Process (NHEP)
// Simplified to run in Scala 2.12 without plotting
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.random._           // For random number generators
import scala.collection.mutable.ArrayBuffer
import scala.math._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Class representing a Non-Homogeneous Erlang Process
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class NH_ErlangProcess(tMax: Double, lambdaf: Double => Double, k: Int = 2, stream: Int = 0) {

  private val baseExp = Exponential(1.0, stream)  // Base exponential generator for Erlang
  private var arrivalTimes = ArrayBuffer[Double]() // Stores generated arrival times

  // Generate arrival times using thinning algorithm
  def gen: Vector[Double] = {
    arrivalTimes.clear()
    var now = 0.0
    val lambdaMax = (0.0 to tMax by 0.1).map(lambdaf).max  // Max lambda(t) for thinning

    while (now < tMax) {
      val erlangIA = (1 to k).map(_ => baseExp.gen).sum / lambdaMax
      now += erlangIA
      if (now <= tMax && math.random <= lambdaf(now) / lambdaMax) arrivalTimes += now
    }

    arrivalTimes.toVector
  }

  // Return number of arrivals by time tt
  def num(tt: Double): Int = {
    if (arrivalTimes.isEmpty) gen
    arrivalTimes.count(_ <= tt)
  }

  // Return flow (number of arrivals) per interval t_span
  def flow(t_span: Double): Vector[Int] = {
    if (arrivalTimes.isEmpty) gen
    val flw = ArrayBuffer[Int]()
    var now = 0.0
    var lastCount = 0

    while (now <= tMax) {
      val count = num(now)
      flw += count - lastCount
      lastCount = count
      now += t_span
    }

    flw.toVector
  }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Main function to run NH_ErlangProcess simulation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
@main def nH_ErlangProcessTest(): Unit = {

  println("=== Running Non-Homogeneous Erlang Process Simulation ===\n")

  val t_end = 50.0  // Total simulation time

  // Lambda function varying over time (peak at t=25)
  def lambdaf(t: Double): Double = 1.5 - 0.001 * pow(t - 25.0, 2)

  // Create NH_ErlangProcess instance
  val nhEp = new NH_ErlangProcess(t_end, lambdaf, k = 2)

  // Generate arrival times
  val arrivals = nhEp.gen
  println(s"Generated Arrival Times:\n$arrivals\n")

  // Number of arrivals at specific times
  println(s"Number of arrivals by t=10: ${nhEp.num(10.0)}")
  println(s"Number of arrivals by t=25: ${nhEp.num(25.0)}")
  println(s"Number of arrivals by t=40: ${nhEp.num(40.0)}\n")

  // Compute flow per 5-minute intervals
  val flow5 = nhEp.flow(5.0)
  println(s"Flow per 5-minute interval: $flow5\n")

  println("=== Simulation Finished ===")
}
