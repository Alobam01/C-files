//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Standalone Scala 2.12 Vehicle Source Simulation
// Author: Adapted from John Miller
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.concurrent.duration._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Vehicle entity
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
case class Vehicle(name: String, var disp: Double = 0.0)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Variate trait for inter-arrival times
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
trait Variate {
  def gen: Double
}

class Uniform(min: Double, max: Double) extends Variate {
  private val rnd = new Random()
  def gen: Double = min + (max - min) * rnd.nextDouble()
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Model class to control simulation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class Model(val name: String) {
  private var currentTime: Double = 0.0
  private val events = ArrayBuffer[(Double, () => Unit)]()

  def schedule(delay: Double)(action: => Unit): Unit = {
    events += ((currentTime + delay, () => action))
  }

  def run(): Unit = {
    while (events.nonEmpty) {
      val (time, action) = events.minBy(_._1)
      currentTime = time
      events -= ((time, action))
      action()
    }
  }

  def time: Double = currentTime
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// VSource class: generates vehicles at inter-arrival times
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class VSource(val name: String, val model: Model, val makeVehicle: () => Vehicle,
              val units: Int, val iArrivalTime: Variate) {

  private var generated = 0

  def start(): Unit = {
    scheduleNext()
  }

  private def scheduleNext(): Unit = {
    if (generated < units) {
      val vehicle = makeVehicle()
      println(f"[t=${model.time}%.2f] Generated vehicle: ${vehicle.name}")
      generated += 1
      model.schedule(iArrivalTime.gen)(scheduleNext())
    }
  }

}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Simulation Driver
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
object VSourceSim extends App {

  val carModel = new Model("CarModel")

  // Factory function for vehicles
  var carCounter = 1
  def makeCar(): Vehicle = {
    val car = Vehicle(s"Car$carCounter")
    carCounter += 1
    car
  }

  // Create a vehicle source
  val source = new VSource("CarMaker", carModel, makeCar, units = 5, new Uniform(1.0, 3.0))

  // Start the source
  source.start()

  // Run the simulation
  println("Starting simulation...")
  carModel.run()
  println("Simulation finished.")

}
