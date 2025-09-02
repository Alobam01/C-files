//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// BankSimulation.scala
// Standalone Scala 2.12 Scalation Simulation - Bank (M/M/1 Queue)
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.random.Exponential
import scalation.random.RandomSeeds.N_STREAMS
import scalation.linalgebra.MatrixD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// BankModel: M/M/1 Queue (single teller)
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class BankModel(name: String, m: Int, rv: Array[Exponential], label_ : Array[String] = null) {

  // Column labels
  val label: Array[String] = if (label_ != null) label_
  else Array("ID-0", "IArrival-1", "Arrival-2",
    "Begin-3", "Wait-4", "Service-5",
    "Departure-6", "Total-7")

  private val e_a = 2  // Arrival column
  private val e_d = 6  // Departure column
  private val mm = label.length

  val tab = MatrixD(m + 1, mm)

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Perform Bank simulation
  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def simulate(): Unit = {
    tab(0, 2) = 0.0 // initial time

    for (i <- 1 to m) {
      tab(i, 0) = i                       // ID
      tab(i, 1) = rv(0).gen               // inter-arrival time
      tab(i, 2) = tab(i - 1, 2) + tab(i, 1) // arrival time
      tab(i, 3) = tab(i, 2) max tab(i - 1, 6) // begin service
      tab(i, 4) = tab(i, 3) - tab(i, 2)       // wait time
      tab(i, 5) = rv(1).gen                   // service time
      tab(i, 6) = tab(i, 3) + tab(i, 5)       // departure time
      tab(i, 7) = tab(i, 6) - tab(i, 2)       // total time in system
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
  // Summarize simulation statistics
  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def summary(): Unit = {
    val avgWait = tab(1 to m, 4).sum / m
    val avgService = tab(1 to m, 5).sum / m
    val avgTotal = tab(1 to m, 7).sum / m

    println(f"\nSummary for $m customers:")
    println(f"Average wait time      = $avgWait%2.3f")
    println(f"Average service time   = $avgService%2.3f")
    println(f"Average total in system= $avgTotal%2.3f")
  }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Main function to run the Bank simulation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
object RunBankTest extends App {
  val HOUR = 60.0      // for exponential time conversion
  val stream = 0
  val maxCusts = 100
  val lambda = 6.0     // arrivals per hour
  val mu = 7.5         // service per hour

  val iArrivalRV = Exponential(HOUR / lambda, stream)
  val serviceRV  = Exponential(HOUR / mu, (stream + 1) % N_STREAMS)

  val bank = new BankModel("Bank M/M/1", maxCusts, Array(iArrivalRV, serviceRV))
  bank.simulate()
  bank.report()
  bank.summary()
}
