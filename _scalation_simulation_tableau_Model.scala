//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// MM1QueueSimulation.scala
// Standalone Scala 2.12 Scalation Simulation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.random.Randi
import scalation.random.RandomSeeds.N_STREAMS
import scalation.linalgebra.MatrixD
import scala.collection.mutable.ArrayBuffer

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Model class for a G/G/1 Queue using tableau simulation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class Model(name: String, m: Int, rv: Array[Randi], label_ : Array[String] = null) {

  // Column labels
  val label: Array[String] = if (label_ != null) label_
  else Array("ID-0", "IArrival-1", "Arrival-2",
    "Begin-3", "Wait-4", "Service-5",
    "Departure-6", "Total-7")

  private val e_a = 2  // Arrival column
  private val e_d = 6  // Departure column
  private val mm = label.length

  // Table holding timing information
  val tab = MatrixD(m + 4, mm)

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Perform simulation
  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def simulate(startTime: Double = 0.0): Unit = {
    tab(0, 2) = startTime
    for (i <- 1 to m) {
      tab(i, 0) = i                              // ID
      tab(i, 1) = rv(0).gen                       // Inter-arrival
      tab(i, 2) = tab(i - 1, 2) + tab(i, 1)      // Arrival
      tab(i, 3) = tab(i, 2) max tab(i - 1, 6)    // Begin service
      tab(i, 4) = tab(i, 3) - tab(i, 2)          // Wait
      tab(i, 5) = rv(1).gen                       // Service
      tab(i, 6) = tab(i, 3) + tab(i, 5)          // Departure
      tab(i, 7) = tab(i, 6) - tab(i, 2)          // Total time in system
    }
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Print the simulation table
  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def report(): Unit = {
    println(s"\n$name Simulation Report")
    println("-" * 80)
    println(label.mkString("\t"))
    println("-" * 80)
    for (i <- 1 to m) {
      println(tab(i).map(d => f"$d%10.3f").mkString("\t"))
    }
    println("-" * 80)
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Summarize the simulation (average queue, service, system time)
  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def summary(): Unit = {
    val Lq = tab(1 to m, 4).sum / m
    val Ls = tab(1 to m, 5).sum / m
    val Ly = tab(1 to m, 7).sum / m

    println(f"\nSummary for $m customers:")
    println(f"Average time in queue  Lq = $Lq%2.3f")
    println(f"Average service time   Ls = $Ls%2.3f")
    println(f"Average total in system Ly = $Ly%2.3f")
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Produce timeline of system occupancy
  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def timeLine(): (Array[Double], Array[Int]) = {
    val et = ArrayBuffer(0.0)
    val lt = ArrayBuffer(0)
    var n = 0

    val departures = tab(1 to m, e_d)
    val arrivals   = tab(1 to m, e_a)

    var i = 0
    var j = 0

    while (i < m || j < m) {
      if (i < m && (j >= m || arrivals(i) <= departures(j))) {
        n += 1
        et += arrivals(i)
        lt += n
        i += 1
      } else {
        n -= 1
        et += departures(j)
        lt += n
        j += 1
      }
    }

    (et.toArray, lt.toArray)
  }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Companion object for helper methods
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
object Model {
  def occupancy(et_lt: (Array[Double], Array[Int])): Unit = {
    val (et, lt) = et_lt
    val sumLy = lt.sum
    val avgLy = sumLy.toDouble / et.length
    println(s"\nOccupancy Timeline: ${lt.mkString(", ")}")
    println(s"Average number in system: $avgLy%2.3f")
  }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Main function to run the simulation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
object RunModelTest extends App {
  val stream = 0
  val maxCusts = 20

  val iArrivalRV = Randi(1, 10, stream)                   // inter-arrival time
  val serviceRV  = Randi(1, 10, (stream + 1) % N_STREAMS) // service time

  val model = new Model("G/G/1 Queue", maxCusts, Array(iArrivalRV, serviceRV))

  model.simulate()
  model.report()
  model.summary()
  Model.occupancy(model.timeLine())
}
