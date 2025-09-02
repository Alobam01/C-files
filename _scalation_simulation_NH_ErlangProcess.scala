//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Standalone Scalation Simulation: Non-Homogeneous Erlang Process (NHEP)
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.random._           // For random number generators (Exponential, etc.)
import scalation.plot._             // For plotting results
import scala.collection.mutable.ArrayBuffer
import scala.math._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Class representing a Non-Homogeneous Erlang Process
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class NH_ErlangProcess(tMax: Double, lambdaf: Double => Double, k: Int = 2, stream: Int = 0) {

  private val baseExp = Exponential(1.0, stream)  // Base exponential generator for Erlang interarrival times
  private var arrivalTimes = ArrayBuffer[Double]() // Stores generated arrival times

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Generate arrival times using the thinning algorithm for NH Erlang process
   *  @return Vector of arrival times
   */
  def gen: Vector[Double] = {
    arrivalTimes.clear()
    var now = 0.0
    val lambdaMax = (0.0 to tMax by 0.1).map(lambdaf).max  // Upper bound of lambda(t) over time

    while (now < tMax) {
      // Erlang interarrival time: sum of k exponentials
      val erlangIA = (1 to k).map(_ => baseExp.gen).sum / lambdaMax
      now += erlangIA
      val d = math.random
      // Thinning: accept arrival with probability lambda(t)/lambdaMax
      if (d <= lambdaf(now) / lambdaMax && now <= tMax) arrivalTimes += now
    }

    arrivalTimes.toVector
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return number of arrivals by a specific time tt, N(tt)
   *  @param tt  time at which to count arrivals
   *  @return number of arrivals up to time tt
   */
  def num(tt: Double): Int = {
    if (arrivalTimes.isEmpty) gen
    arrivalTimes.indexWhere(_ > tt) match {
      case -1 => arrivalTimes.length  // All arrivals occurred before tt
      case i  => i                    // First arrival after tt
    }
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return the flow (number of arrivals) per interval of length t_span
   *  @param t_span  length of each time interval
   *  @return Vector of arrivals per interval
   */
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
// Main function to run the NH_ErlangProcess simulation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
@main def nH_ErlangProcessTest(): Unit = {

  println("=== Running Non-Homogeneous Erlang Process Simulation ===\n")

  val t_end = 50.0  // Total simulation time (e.g., minutes)

  // Lambda function varying over time (peaks at t=25)
  def lambdaf(t: Double): Double = 1.5 - 0.001 * pow(t - 25.0, 2)

  // Create NH_ErlangProcess instance with k=2 (Erlang shape parameter)
  val nhEp = new NH_ErlangProcess(t_end, lambdaf, k = 2)

  // Generate arrival times
  val arrivals = nhEp.gen
  println(s"Generated Arrival Times: $arrivals\n")

  // Number of arrivals by specific times
  println(s"Number of arrivals by t=10: ${nhEp.num(10.0)}")
  println(s"Number of arrivals by t=25: ${nhEp.num(25.0)}")
  println(s"Number of arrivals by t=40: ${nhEp.num(40.0)}\n")

  // Compute flow per 5-minute intervals
  val flow5 = nhEp.flow(5.0)
  println(s"Flow per 5-minute interval: $flow5\n")

  // Plot flow over time
  val tflow = Vector.range(0, flow5.length).map(_ * 5.0) // Time points for intervals
  new Plot(tflow, flow5.map(_.toDouble), null, "NHEP: Arrivals per 5 min", lines = true)

  println("=== Simulation Finished ===")
}
