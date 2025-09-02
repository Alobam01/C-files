//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Standalone Scala 2.12 Variable-Speed Transport Simulation
// Author: Adapted from John Miller & Casey Bowman
// Date: 2025-08-29
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scala.collection.mutable.ArrayDeque
import scala.util.Random

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// SimActor / Vehicle represents an entity moving along the transport
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
case class Vehicle(name: String, var disp: Double = 0.0, speed: Double = 1.0, var rt: Double = 1.0) {
  def schedule(dt: Double): Unit = {
    rt = dt // set remaining time (for demonstration)
  }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Simple Dynamics model for variable speed
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class Dynamics {
  def updateV(vehicle: Vehicle, length: Double): Unit = {
    vehicle.disp += vehicle.speed
    if (vehicle.disp > length) vehicle.disp = length
  }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Component stub for simulation nodes
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class Component(val name: String)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// VTransport class: Variable-speed transport between two components
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class VTransport(val name: String,
                 val from: Component,
                 val to: Component,
                 val motion: Dynamics,
                 val length: Double = 10.0) {

  private val vehicles = ArrayDeque[Vehicle]() // vehicles in transport
  private var done: Boolean = false

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Add vehicle to transport
  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def addVehicle(vehicle: Vehicle): Unit = {
    vehicles += vehicle
    println(s"Vehicle ${vehicle.name} entered transport '$name'")
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Move vehicles along transport
  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def moveVehicles(): Unit = {
    done = false
    while (!done && vehicles.nonEmpty) {
      for (vehicle <- vehicles) {
        motion.updateV(vehicle, length)
        println(f"Vehicle ${vehicle.name} moved to disp = ${vehicle.disp}%.2f")

        if (vehicle.disp >= length) {
          println(s"Vehicle ${vehicle.name} reached end of transport '$name'")
          vehicles -= vehicle
        }
      }
      if (vehicles.isEmpty) done = true
    }
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Get first vehicle in transport
  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def getFirst: Option[Vehicle] = vehicles.headOption

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Get last vehicle in transport
  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def getLast: Option[Vehicle] = vehicles.lastOption

} // VTransport

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Simulation Driver
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
object VTransportSim extends App {

  val compA = new Component("A")
  val compB = new Component("B")
  val dynamics = new Dynamics

  val transport = new VTransport("Road1", compA, compB, dynamics, length = 15.0)

  // Create vehicles
  val vehicles = Seq(
    Vehicle("Car1", speed = 2.0),
    Vehicle("Car2", speed = 1.5),
    Vehicle("Car3", speed = 1.0)
  )

  // Add vehicles to transport
  vehicles.foreach(transport.addVehicle)

  // Move vehicles along transport
  println("\nSimulating vehicle movements...")
  transport.moveVehicles()

  println(s"\nFirst vehicle remaining: ${transport.getFirst.map(_.name).getOrElse("None")}")
  println(s"Last vehicle remaining:  ${transport.getLast.map(_.name).getOrElse("None")}")

}
