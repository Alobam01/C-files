//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// CallCenterSimulation.scala
// Standalone Scala 2.12 Scalation Simulation - Call Center (M/M/1/1 Queue)
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.random.Randi
import scalation.random.RandomSeeds.N_STREAMS
import scalation.linalgebra.MatrixD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// CallCenterModel: M/M/1/1 Queue (no waiting - blocked calls lost)
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class CallCenterModel(name: String, m: Int, rv: Array[Randi], label_ : Array[String] = null) {

  // Column labels
  val label: Array[String] = if (label_ != null) label_
  else Array("ID-0", "IArrival-1", "Arrival-2",
    "Begin-3", "Wait-4", "Service-5",
    "Departure-6", "Total-7")

  private val e_a = 2  // Arrival column
  private val e_d = 6  // Departure column
  private val mm = label.length

  val tab = MatrixD(m + 1, mm)
  var lastEstablishedCall = 0

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Perform Call Center simulation
  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def simulate(): Unit = {
    tab(0, 2) = 0.0 // initial time

    for (i <- 1 to m) {
      tab(i, 0) = i                       // ID
      tab(i, 1) = rv(0).gen               // inter-arrival
      tab(i, 2) = tab(i - 1, 2) + tab(i, 1) // arrival

      // check if previous call is finished
      if (lastEstablishedCall == 0 || tab(lastEstablishedCall, e_d) <= tab(i, 2)) {
        tab(i, 3) = tab(i, 2)             // begin service
        tab(i, 4) = tab(i, 3) - tab(i, 2) // wait time
        tab(i, 5) = rv(1).gen             // service time
        tab(i, 6) = tab(i, 3) + tab(i, 5) // departure
        tab(i, 7) = tab(i, 6) - tab(i, 2) // total time in system
        lastEstablishedCall = i
      } else {
        // call is lost, mark zero for service
        tab(i, 3) = tab(i, 2)
        tab(i, 4) = 0.0
        tab(i, 5) = 0.0
        tab(i, 6) = tab(lastEstablishedCall, e_d)
        tab(i, 7) = 0.0
      }
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
  // Summarize the simulation
  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def summary(): Unit = {
    val served = tab(1 to m, 5).filter(_ > 0)
    val avgWait = tab(1 to m, 4).sum / m
    val avgService = served.sum / served.length
    val avgTotal = tab(1 to m, 7).sum / served.length

    println(f"\nSummary for $m calls:")
    println(f"Average wait time      = $avgWait%2.3f")
    println(f"Average service time   = $avgService%2.3f")
    println(f"Average total in system= $avgTotal%2.3f")
    println(f"Calls served           = ${served.length}")
    println(f"Calls lost             = ${m - served.length}")
  }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Main function to run the simulation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
object RunCallCenterTest extends App {
  val stream = 0
  val maxCalls = 20

  val iArrivalRV = Randi(1, 10, stream)                   // inter-arrival time
  val serviceRV  = Randi(1, 10, (stream + 1) % N_STREAMS) // service time

  val callCenter = new CallCenterModel("Call Center M/M/1/1", maxCalls, Array(iArrivalRV, serviceRV))

  callCenter.simulate()
  callCenter.report()
  callCenter.summary()
}
