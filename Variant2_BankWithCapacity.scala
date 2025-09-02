//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// BankWithCapacitySimulation.scala
// Standalone Scala 2.12 Scalation Simulation - Bank with Limited Capacity (M/M/1/K Queue)
// Variant 2: Parameter Tuning - Different distributions, queue capacity, multiple tellers
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.random.Exponential
import scalation.random.RandomSeeds.N_STREAMS
import scalation.linalgebra.MatrixD
import scala.collection.mutable.Queue

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// BankWithCapacityModel: M/M/2/15 Queue (2 tellers, max 15 customers)
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class BankWithCapacityModel(name: String, m: Int, rv: Array[Exponential], 
                           queueCapacity: Int = 15, numTellers: Int = 2, 
                           label_ : Array[String] = null) {

  // Enhanced column labels
  val label: Array[String] = if (label_ != null) label_
  else Array("ID-0", "IArrival-1", "Arrival-2", "QueueJoin-3",
    "Begin-4", "Wait-5", "Service-6", "TellerID-7",
    "Departure-8", "Total-9", "Balked-10")

  private val mm = label.length
  val tab = MatrixD(m + 1, mm)
  private var balkedCustomers = 0
  private val waitingQueue = Queue[Int]()
  private val tellerBusyUntil = Array.fill(numTellers)(0.0)

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Find next available teller
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  private def findAvailableTeller(currentTime: Double): Int = {
    var earliestTeller = 0
    var earliestTime = tellerBusyUntil(0)
    
    for (i <- 1 until numTellers) {
      if (tellerBusyUntil(i) < earliestTime) {
        earliestTime = tellerBusyUntil(i)
        earliestTeller = i
      }
    }
    earliestTeller
  }

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Perform enhanced Bank simulation with capacity and multiple tellers
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def simulate(): Unit = {
    tab(0, 2) = 0.0 // initial time

    for (i <- 1 to m) {
      tab(i, 0) = i                       // ID
      tab(i, 1) = rv(0).gen               // inter-arrival time
      tab(i, 2) = tab(i - 1, 2) + tab(i, 1) // arrival time
      
      // Check if queue is at capacity
      val currentQueueSize = waitingQueue.size
      val busyTellers = tellerBusyUntil.count(_ > tab(i, 2))
      
      if (currentQueueSize + busyTellers >= queueCapacity) {
        // Customer balks (leaves without service)
        tab(i, 3) = -1  // Queue join time (negative indicates balking)
        tab(i, 4) = -1  // Begin service
        tab(i, 5) = -1  // Wait time
        tab(i, 6) = -1  // Service time
        tab(i, 7) = -1  // Teller ID
        tab(i, 8) = -1  // Departure time
        tab(i, 9) = -1  // Total time
        tab(i, 10) = 1  // Balked indicator
        balkedCustomers += 1
      } else {
        // Customer joins the system
        tab(i, 3) = tab(i, 2)  // Queue join time
        
        val assignedTeller = findAvailableTeller(tab(i, 2))
        val serviceStartTime = tab(i, 2) max tellerBusyUntil(assignedTeller)
        
        tab(i, 4) = serviceStartTime                    // Begin service
        tab(i, 5) = serviceStartTime - tab(i, 2)        // Wait time
        tab(i, 6) = rv(1).gen                          // Service time
        tab(i, 7) = assignedTeller + 1                 // Teller ID (1-indexed)
        tab(i, 8) = serviceStartTime + tab(i, 6)       // Departure time
        tab(i, 9) = tab(i, 8) - tab(i, 2)              // Total time in system
        tab(i, 10) = 0                                 // Not balked
        
        // Update teller availability
        tellerBusyUntil(assignedTeller) = tab(i, 8)
      }
    }
  }

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Print the simulation table
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def report(): Unit = {
    println(s"\n$name Simulation Report (Capacity: $queueCapacity, Tellers: $numTellers)")
    println("-" * 120)
    println(label.mkString("\t"))
    println("-" * 120)
    for (i <- 1 to m) {
      println(tab(i).map(d => if (d == -1) "BALKED" else f"$d%8.3f").mkString("\t"))
    }
    println("-" * 120)
  }

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Enhanced summary with balking statistics
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def summary(): Unit = {
    val servedCustomers = m - balkedCustomers
    val servedIndices = (1 to m).filter(i => tab(i, 10) == 0)
    
    if (servedCustomers > 0) {
      val avgWait = servedIndices.map(i => tab(i, 5)).sum / servedCustomers
      val avgService = servedIndices.map(i => tab(i, 6)).sum / servedCustomers
      val avgTotal = servedIndices.map(i => tab(i, 9)).sum / servedCustomers
      val balkingRate = balkedCustomers.toDouble / m * 100

      println(f"\nEnhanced Summary for $m customers:")
      println(f"Customers served           = $servedCustomers")
      println(f"Customers balked           = $balkedCustomers")
      println(f"Balking rate               = $balkingRate%2.1f%%")
      println(f"Average wait time (served) = $avgWait%2.3f")
      println(f"Average service time       = $avgService%2.3f")
      println(f"Average total in system    = $avgTotal%2.3f")
    } else {
      println("\nAll customers balked - system overloaded!")
    }
  }
}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Main function to run the enhanced Bank simulation
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
object RunBankCapacityTest extends App {
  val HOUR = 60.0      // for exponential time conversion
  val stream = 0
  val maxCusts = 150   // More customers to test capacity
  val lambda = 12.0    // Higher arrival rate (arrivals per hour)
  val mu = 8.0         // Slower service rate (service per hour)
  val capacity = 15    // Queue capacity
  val tellers = 2      // Number of tellers

  val iArrivalRV = Exponential(HOUR / lambda, stream)
  val serviceRV  = Exponential(HOUR / mu, (stream + 1) % N_STREAMS)

  val bank = new BankWithCapacityModel("Enhanced Bank M/M/2/15", maxCusts, 
                                      Array(iArrivalRV, serviceRV), capacity, tellers)
  bank.simulate()
  bank.report()
  bank.summary()
}