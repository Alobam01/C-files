//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// HospitalERSimulation.scala
// Standalone Scala 2.12 Scalation Simulation - Hospital Emergency Room (M/M/1 Queue)
// Variant 1: Rename & Refactor - Customer → Patient, Bank → Hospital
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.random.Exponential
import scalation.random.RandomSeeds.N_STREAMS
import scalation.linalgebra.MatrixD

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// HospitalERModel: M/M/1 Queue (single doctor)
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class HospitalERModel(facilityName: String, patientCount: Int, randomVariates: Array[Exponential], columnLabels: Array[String] = null) {

  // Column labels for patient tracking
  val labels: Array[String] = if (columnLabels != null) columnLabels
  else Array("PatientID-0", "InterArrival-1", "ArrivalTime-2",
    "TreatmentStart-3", "WaitingTime-4", "TreatmentDuration-5",
    "DischargeTime-6", "TotalTimeInER-7")

  private val arrivalCol = 2  // Arrival time column
  private val dischargeCol = 6  // Discharge time column
  private val numColumns = labels.length

  val patientTable = MatrixD(patientCount + 1, numColumns)

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Perform Hospital ER simulation
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def runSimulation(): Unit = {
    patientTable(0, 2) = 0.0 // initial time

    for (patientNum <- 1 to patientCount) {
      patientTable(patientNum, 0) = patientNum                       // Patient ID
      patientTable(patientNum, 1) = randomVariates(0).gen           // inter-arrival time
      patientTable(patientNum, 2) = patientTable(patientNum - 1, 2) + patientTable(patientNum, 1) // arrival time
      patientTable(patientNum, 3) = patientTable(patientNum, 2) max patientTable(patientNum - 1, 6) // treatment start
      patientTable(patientNum, 4) = patientTable(patientNum, 3) - patientTable(patientNum, 2)       // waiting time
      patientTable(patientNum, 5) = randomVariates(1).gen           // treatment duration
      patientTable(patientNum, 6) = patientTable(patientNum, 3) + patientTable(patientNum, 5)       // discharge time
      patientTable(patientNum, 7) = patientTable(patientNum, 6) - patientTable(patientNum, 2)       // total time in ER
    }
  }

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Print the simulation results
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def generateReport(): Unit = {
    println(s"\n$facilityName Emergency Room Simulation Report")
    println("-" * 90)
    println(labels.mkString("\t"))
    println("-" * 90)
    for (patientNum <- 1 to patientCount) {
      println(patientTable(patientNum).map(d => f"$d%10.3f").mkString("\t"))
    }
    println("-" * 90)
  }

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Generate summary statistics
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def printSummary(): Unit = {
    val avgWaitingTime = patientTable(1 to patientCount, 4).sum / patientCount
    val avgTreatmentTime = patientTable(1 to patientCount, 5).sum / patientCount
    val avgTotalTimeInER = patientTable(1 to patientCount, 7).sum / patientCount

    println(f"\nStatistical Summary for $patientCount patients:")
    println(f"Average waiting time       = $avgWaitingTime%2.3f minutes")
    println(f"Average treatment time     = $avgTreatmentTime%2.3f minutes")
    println(f"Average total time in ER   = $avgTotalTimeInER%2.3f minutes")
  }
}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Main function to run the Hospital ER simulation
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
object RunHospitalERTest extends App {
  val MINUTE = 1.0     // time unit in minutes
  val streamIndex = 0
  val maxPatients = 100
  val arrivalRate = 4.0     // patients per hour
  val treatmentRate = 5.0   // patients per hour

  val patientArrivalRV = Exponential(60.0 / arrivalRate, streamIndex)
  val treatmentTimeRV  = Exponential(60.0 / treatmentRate, (streamIndex + 1) % N_STREAMS)

  val hospitalER = new HospitalERModel("City General Hospital", maxPatients, Array(patientArrivalRV, treatmentTimeRV))
  hospitalER.runSimulation()
  hospitalER.generateReport()
  hospitalER.printSummary()
}