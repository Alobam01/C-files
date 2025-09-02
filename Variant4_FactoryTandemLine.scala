//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// FactoryTandemLineSimulation.scala
// Standalone Scala 2.12 Scalation Simulation - Factory Production Line (Tandem Queue)
// Variant 4: Component Swap - Single queue → Two-stage tandem production line
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.random.Exponential
import scalation.random.RandomSeeds.N_STREAMS
import scalation.linalgebra.MatrixD
import scala.collection.mutable.Queue

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// FactoryTandemLineModel: Two-stage production line (Inspection → Packaging)
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class FactoryTandemLineModel(factoryName: String, productCount: Int, rv: Array[Exponential], 
                            inspectionCapacity: Int = 5, packagingCapacity: Int = 3,
                            label_ : Array[String] = null) {

  // Enhanced column labels for two-stage process
  val label: Array[String] = if (label_ != null) label_
  else Array("ProductID-0", "InterArrival-1", "ArrivalTime-2",
    "InspectionStart-3", "InspectionWait-4", "InspectionTime-5", "InspectionEnd-6",
    "PackagingStart-7", "PackagingWait-8", "PackagingTime-9", "PackagingEnd-10",
    "TotalSystemTime-11", "QualityPass-12")

  private val mm = label.length
  val productionLog = MatrixD(productCount + 1, mm)
  
  // Two-stage queues
  private val inspectionQueue = Queue[Int]()
  private val packagingQueue = Queue[Int]()
  
  // Station availability tracking
  private val inspectionStationBusy = Array.fill(inspectionCapacity)(0.0)
  private val packagingStationBusy = Array.fill(packagingCapacity)(0.0)
  
  // Quality control metrics
  private var passedInspection = 0
  private var rejectedProducts = 0
  private val qualityPassRate = 0.95  // 95% pass rate

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Find next available station in a given stage
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  private def findAvailableStation(stationBusyTimes: Array[Double], currentTime: Double): Int = {
    var earliestStation = 0
    var earliestTime = stationBusyTimes(0)
    
    for (i <- 1 until stationBusyTimes.length) {
      if (stationBusyTimes(i) < earliestTime) {
        earliestTime = stationBusyTimes(i)
        earliestStation = i
      }
    }
    earliestStation
  }

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Simulate quality inspection (some products may be rejected)
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  private def performQualityCheck(): Boolean = {
    scala.util.Random.nextDouble() < qualityPassRate
  }

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Perform Factory Tandem Line simulation
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def runProduction(): Unit = {
    productionLog(0, 2) = 0.0 // initial time

    for (i <- 1 to productCount) {
      // Product arrival
      productionLog(i, 0) = i                       // Product ID
      productionLog(i, 1) = rv(0).gen               // inter-arrival time
      productionLog(i, 2) = productionLog(i - 1, 2) + productionLog(i, 1) // arrival time
      
      // STAGE 1: INSPECTION
      val inspectionStation = findAvailableStation(inspectionStationBusy, productionLog(i, 2))
      val inspectionStartTime = productionLog(i, 2) max inspectionStationBusy(inspectionStation)
      
      productionLog(i, 3) = inspectionStartTime                    // inspection start
      productionLog(i, 4) = inspectionStartTime - productionLog(i, 2)  // inspection wait
      productionLog(i, 5) = rv(1).gen                             // inspection time
      productionLog(i, 6) = inspectionStartTime + productionLog(i, 5)  // inspection end
      
      // Update inspection station availability
      inspectionStationBusy(inspectionStation) = productionLog(i, 6)
      
      // Quality check
      val passesQuality = performQualityCheck()
      productionLog(i, 12) = if (passesQuality) 1.0 else 0.0
      
      if (passesQuality) {
        passedInspection += 1
        
        // STAGE 2: PACKAGING (only for products that pass inspection)
        val packagingStation = findAvailableStation(packagingStationBusy, productionLog(i, 6))
        val packagingStartTime = productionLog(i, 6) max packagingStationBusy(packagingStation)
        
        productionLog(i, 7) = packagingStartTime                    // packaging start
        productionLog(i, 8) = packagingStartTime - productionLog(i, 6)  // packaging wait
        productionLog(i, 9) = rv(2).gen                             // packaging time
        productionLog(i, 10) = packagingStartTime + productionLog(i, 9)  // packaging end
        productionLog(i, 11) = productionLog(i, 10) - productionLog(i, 2)  // total system time
        
        // Update packaging station availability
        packagingStationBusy(packagingStation) = productionLog(i, 10)
      } else {
        // Product rejected - no packaging stage
        rejectedProducts += 1
        productionLog(i, 7) = -1   // No packaging start
        productionLog(i, 8) = -1   // No packaging wait
        productionLog(i, 9) = -1   // No packaging time
        productionLog(i, 10) = -1  // No packaging end
        productionLog(i, 11) = productionLog(i, 6) - productionLog(i, 2)  // Total time (inspection only)
      }
    }
  }

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Print the production simulation table
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def generateProductionReport(): Unit = {
    println(s"\n$factoryName - Two-Stage Production Line Report")
    println(s"Inspection Stations: $inspectionCapacity, Packaging Stations: $packagingCapacity")
    println("=" * 140)
    println(label.mkString("\t"))
    println("=" * 140)
    
    for (i <- 1 to productCount) {
      val qualityStatus = if (productionLog(i, 12) == 1.0) "PASS" else "REJECT"
      val row = productionLog(i).map(d => if (d == -1) "N/A" else f"$d%8.3f").mkString("\t")
      println(s"$row\t$qualityStatus")
    }
    println("=" * 140)
  }

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Generate comprehensive production summary
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def productionSummary(): Unit = {
    val completedProducts = passedInspection
    val completedIndices = (1 to productCount).filter(i => productionLog(i, 12) == 1.0)
    
    if (completedProducts > 0) {
      val avgInspectionWait = completedIndices.map(i => productionLog(i, 4)).sum / completedProducts
      val avgInspectionTime = completedIndices.map(i => productionLog(i, 5)).sum / completedProducts
      val avgPackagingWait = completedIndices.map(i => productionLog(i, 8)).sum / completedProducts
      val avgPackagingTime = completedIndices.map(i => productionLog(i, 9)).sum / completedProducts
      val avgTotalTime = completedIndices.map(i => productionLog(i, 11)).sum / completedProducts
      
      val actualQualityRate = (passedInspection.toDouble / productCount) * 100
      val throughput = completedProducts.toDouble / productionLog(productCount, 10) * 60  // products per hour

      println(f"\n🏭 $factoryName Production Summary:")
      println("=" * 50)
      println(f"Total products processed   = $productCount")
      println(f"Products passed inspection = $passedInspection")
      println(f"Products rejected          = $rejectedProducts")
      println(f"Quality pass rate          = $actualQualityRate%2.1f%%")
      println(f"System throughput          = $throughput%2.1f products/hour")
      println()
      println("📊 Stage Performance:")
      println(f"Avg inspection wait time   = $avgInspectionWait%2.3f minutes")
      println(f"Avg inspection process time= $avgInspectionTime%2.3f minutes")
      println(f"Avg packaging wait time    = $avgPackagingWait%2.3f minutes")
      println(f"Avg packaging process time = $avgPackagingTime%2.3f minutes")
      println(f"Avg total system time      = $avgTotalTime%2.3f minutes")
      
      // Bottleneck analysis
      val inspectionUtilization = avgInspectionTime / (avgInspectionTime + avgInspectionWait) * 100
      val packagingUtilization = avgPackagingTime / (avgPackagingTime + avgPackagingWait) * 100
      
      println()
      println("⚙️  Bottleneck Analysis:")
      println(f"Inspection stage utilization = $inspectionUtilization%2.1f%%")
      println(f"Packaging stage utilization  = $packagingUtilization%2.1f%%")
      
      if (avgInspectionWait > avgPackagingWait) {
        println("🔍 Bottleneck: Inspection stage - consider adding more inspection stations")
      } else if (avgPackagingWait > avgInspectionWait) {
        println("📦 Bottleneck: Packaging stage - consider adding more packaging stations")
      } else {
        println("✅ Production line is well-balanced")
      }
    } else {
      println("\n❌ No products completed the full production process!")
    }
  }
}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Main function to run the Factory Tandem Line simulation
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
object RunFactoryTandemTest extends App {
  val MINUTE = 1.0     // time unit in minutes
  val stream = 0
  val totalProducts = 200
  val arrivalRate = 0.8        // products per minute
  val inspectionRate = 1.2     // products per minute per station
  val packagingRate = 1.0      // products per minute per station
  
  val inspectionStations = 3
  val packagingStations = 2

  val productArrivalRV = Exponential(1.0 / arrivalRate, stream)
  val inspectionTimeRV = Exponential(1.0 / inspectionRate, (stream + 1) % N_STREAMS)
  val packagingTimeRV  = Exponential(1.0 / packagingRate, (stream + 2) % N_STREAMS)

  val factory = new FactoryTandemLineModel("TechCorp Manufacturing", totalProducts, 
                                          Array(productArrivalRV, inspectionTimeRV, packagingTimeRV),
                                          inspectionStations, packagingStations)
  
  println("🏭 Starting Factory Tandem Production Line Simulation...")
  factory.runProduction()
  factory.generateProductionReport()
  factory.productionSummary()
  println("\n🎯 Production Simulation Complete!")
}