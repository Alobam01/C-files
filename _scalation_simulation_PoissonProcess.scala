//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Standalone Scalation Simulation: Poisson Process (Car Arrivals Example)
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.random._                 // For Exponential random number generator
import scala.collection.mutable.ArrayBuffer

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Class representing a Poisson Process
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class PoissonProcess(val tMax: Double, val lambda: Double = 1.0, val stream: Int = 0) {

  private val meanIA = 1.0 / lambda                   // Mean interarrival time = 1/λ
  private val t_ia = Exponential(meanIA, stream)      // Exponential random variable for interarrival times
  private var arrivalTimes = ArrayBuffer[Double]()    // Vector to store generated arrival times

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Generate arrival times in the interval [0, tMax] using Exponential interarrival times
   *  @return Vector of arrival times
   */
  def gen: Vector[Double] = {
    var now = 0.0
    arrivalTimes.clear()    // Reset arrival times

    while (now <= tMax) {
      now += t_ia.gen       // Generate next interarrival
      if (now <= tMax) arrivalTimes += now
    }

    arrivalTimes.toVector
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return number of arrivals by a specific time tt
   *  @param tt  Time at which to count arrivals
   *  @return Number of arrivals N(tt)
   */
  def num(tt: Double): Int = {
    if (arrivalTimes.isEmpty) gen
    arrivalTimes.count(_ <= tt)
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return number of arrivals per interval of length tSpan
   *  @param tSpan  Length of each interval
   *  @return Vector of arrivals per interval
   */
  def flow(tSpan: Double): Vector[Int] = {
    if (arrivalTimes.isEmpty) gen
    val flw = ArrayBuffer[Int]()
    var now = 0.0
    var lastCount = 0

    while (now <= tMax) {
      val count = num(now)
      flw += count - lastCount
      lastCount = count
      now += tSpan
    }

    flw.toVector
  }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Main function to run the Poisson Process simulation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
@main def poissonProcessTest(): Unit = {

  println("=== Running Poisson Process Simulation ===\n")

  val t_end = 50.0          // Total simulation time (minutes)
  val lambda = 2.0          // Average number of arrivals per minute

  // Create PoissonProcess instance
  val pp = new PoissonProcess(t_end, lambda)

  // Generate arrival times
  val arrivals = pp.gen
  println(s"Arrival times:\n$arrivals\n")

  // Number of arrivals by specific times
  println(s"Number of arrivals by time t=10: ${pp.num(10.0)}")
  println(s"Number of arrivals by time t=20: ${pp.num(20.0)}")
  println(s"Number of arrivals by time t=30: ${pp.num(30.0)}\n")

  // Compute flow per 5-minute intervals
  val flow5 = pp.flow(5.0)
  println(s"Flow per 5-minute interval: $flow5\n")

  // Optional: text-based histogram of arrivals per interval
  println("Histogram of arrivals per 5-minute interval:")
  flow5.zipWithIndex.foreach { case (count, i) =>
    println(f"${i*5}%2d-${(i+1)*5}%2d min: " + "*" * count)
  }

  println("\n=== Simulation Finished ===")
}
