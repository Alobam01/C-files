//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// AirportCheckinSimulation.scala
// Standalone Scala 2.12 Scalation Simulation - Airport Check-in Counter (M/M/1 Queue)
// Variant 3: Scenario Reskinning - Bank → Airport Check-in Counter
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.random.Exponential
import scalation.random.RandomSeeds.N_STREAMS
import scalation.linalgebra.MatrixD

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// AirportCheckinModel: M/M/1 Queue (single check-in counter)
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class AirportCheckinModel(airportName: String, passengerCount: Int, rv: Array[Exponential], 
                         flightInfo: String = "Flight AA123", label_ : Array[String] = null) {

  // Column labels for passenger processing
  val label: Array[String] = if (label_ != null) label_
  else Array("PassengerID-0", "InterArrival-1", "ArrivalAtCounter-2",
    "CheckinStart-3", "QueueWait-4", "CheckinTime-5",
    "BoardingPassIssued-6", "TotalProcessTime-7")

  private val arrivalCol = 2  // Arrival at counter column
  private val departureCol = 6  // Boarding pass issued column
  private val mm = label.length

  val passengerLog = MatrixD(passengerCount + 1, mm)
  
  // Airport-specific metrics
  private var onTimePassengers = 0
  private val cutoffTime = 120.0  // 2 hours before flight in minutes

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Perform Airport Check-in simulation
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def processPassengers(): Unit = {
    passengerLog(0, 2) = 0.0 // simulation start time

    for (i <- 1 to passengerCount) {
      passengerLog(i, 0) = i                       // Passenger ID
      passengerLog(i, 1) = rv(0).gen               // inter-arrival time
      passengerLog(i, 2) = passengerLog(i - 1, 2) + passengerLog(i, 1) // arrival at counter
      passengerLog(i, 3) = passengerLog(i, 2) max passengerLog(i - 1, 6) // check-in start
      passengerLog(i, 4) = passengerLog(i, 3) - passengerLog(i, 2)       // queue wait time
      passengerLog(i, 5) = rv(1).gen               // check-in processing time
      passengerLog(i, 6) = passengerLog(i, 3) + passengerLog(i, 5)       // boarding pass issued
      passengerLog(i, 7) = passengerLog(i, 6) - passengerLog(i, 2)       // total process time
      
      // Track on-time passengers (completed check-in within cutoff)
      if (passengerLog(i, 6) <= cutoffTime) {
        onTimePassengers += 1
      }
    }
  }

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Print the check-in simulation table
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def generateFlightReport(): Unit = {
    println(s"\n$airportName - $flightInfo Check-in Report")
    println("=" * 90)
    println(label.mkString("\t"))
    println("=" * 90)
    for (i <- 1 to passengerCount) {
      val status = if (passengerLog(i, 6) <= cutoffTime) "ON-TIME" else "LATE"
      println(passengerLog(i).map(d => f"$d%10.3f").mkString("\t") + s"\t$status")
    }
    println("=" * 90)
  }

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Generate airport-specific summary statistics
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def flightSummary(): Unit = {
    val avgQueueWait = passengerLog(1 to passengerCount, 4).sum / passengerCount
    val avgCheckinTime = passengerLog(1 to passengerCount, 5).sum / passengerCount
    val avgTotalTime = passengerLog(1 to passengerCount, 7).sum / passengerCount
    val onTimeRate = (onTimePassengers.toDouble / passengerCount) * 100
    val latePassengers = passengerCount - onTimePassengers
    
    // Calculate peak queue time
    val maxQueueWait = passengerLog(1 to passengerCount, 4).max
    val peakPassenger = (1 to passengerCount).find(i => passengerLog(i, 4) == maxQueueWait).getOrElse(0)

    println(f"\n$flightInfo Check-in Statistics ($passengerCount passengers):")
    println("=" * 60)
    println(f"Average queue wait time    = $avgQueueWait%2.3f minutes")
    println(f"Average check-in time      = $avgCheckinTime%2.3f minutes")
    println(f"Average total process time = $avgTotalTime%2.3f minutes")
    println(f"Maximum queue wait         = $maxQueueWait%2.3f minutes (Passenger #$peakPassenger)")
    println()
    println(f"On-time passengers         = $onTimePassengers ($onTimeRate%2.1f%%)")
    println(f"Late passengers            = $latePassengers")
    println(f"Check-in cutoff time       = $cutoffTime%2.0f minutes before flight")
    
    if (latePassengers > 0) {
      println("\n⚠️  WARNING: Some passengers may miss their flight!")
    } else {
      println("\n✅ All passengers completed check-in on time.")
    }
  }

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Generate operational recommendations
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def operationalRecommendations(): Unit = {
    val avgQueueWait = passengerLog(1 to passengerCount, 4).sum / passengerCount
    val onTimeRate = (onTimePassengers.toDouble / passengerCount) * 100
    
    println("\n📊 Operational Recommendations:")
    println("-" * 40)
    
    if (avgQueueWait > 15.0) {
      println("• Consider opening additional check-in counters")
      println("• Implement online check-in promotion")
    }
    
    if (onTimeRate < 95.0) {
      println("• Recommend earlier passenger arrival")
      println("• Consider express check-in lanes")
    }
    
    if (avgQueueWait < 5.0 && onTimeRate > 98.0) {
      println("• Current staffing level is optimal")
      println("• Consider reducing counter hours during off-peak")
    }
  }
}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Main function to run the Airport Check-in simulation
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
object RunAirportCheckinTest extends App {
  val MINUTE = 1.0     // time unit in minutes
  val stream = 0
  val totalPassengers = 120  // Typical domestic flight capacity
  val arrivalRate = 2.5      // passengers per minute (150 per hour)
  val checkinRate = 3.0      // passengers per minute (180 per hour)

  val passengerArrivalRV = Exponential(1.0 / arrivalRate, stream)
  val checkinProcessRV   = Exponential(1.0 / checkinRate, (stream + 1) % N_STREAMS)

  val airport = new AirportCheckinModel("Denver International Airport", totalPassengers, 
                                       Array(passengerArrivalRV, checkinProcessRV), 
                                       "Flight UA456 to Chicago")
  
  println("🛫 Starting Airport Check-in Simulation...")
  airport.processPassengers()
  airport.generateFlightReport()
  airport.flightSummary()
  airport.operationalRecommendations()
  println("\n✈️  Simulation Complete!")
}