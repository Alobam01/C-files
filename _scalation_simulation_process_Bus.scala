//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Standalone Scalation Simulation: Bus Transport Simulation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.simulation.process._    // For SimActor, Model, WaitQueue
import scalation.random._                // For random number generation
import scala.util.control.Breaks._       // For breaking loops

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Bus Actor: Models a bus in the simulation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class Bus(name: String, director: Model, lTime: Variate, cap: Int)
  extends SimActor(s"bus_$name", director) {

  private var rider = 0                       // Current number of passengers on the bus

  println(s"Bus $name initialized with capacity $cap")

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Load passengers from a queue */
  def load(que: WaitQueue): Unit = {
    val delay = lTime.gen                     // Loading time per passenger
    schedule(delay)                           // Simulate loading time
    breakable {
      for (_ <- 0 until cap if rider < cap) {
        if (que.isEmpty) break()             // Stop if no passengers waiting
        rider += 1                            // Increment passenger count
      }
    }
    println(s"Bus $name loaded $rider passengers at time ${Model.clock}")
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Unload passengers (e.g., at destination) */
  def unload(): Unit = {
    val delay = lTime.gen                     // Unloading time
    schedule(delay)                           // Simulate unloading time
    if (rider > 0) rider -= 1
    println(s"Bus $name unloaded passengers, remaining: $rider at time ${Model.clock}")
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Define the bus behavior as an actor */
  override def act(): Unit = {
    println(s"Bus $name starting operation at time ${Model.clock}")
  }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Passenger Actor: Models a passenger waiting for a bus
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
case class Passenger(name: String = "Passenger") extends SimActor(name) {
  override def act(): Unit = {
    println(s"$name waiting at time ${Model.clock}")
    wait(Uniform(1.0, 3.0)())               // Simulated waiting time
    println(s"$name ready to board at time ${Model.clock}")
  }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// BusSimulationModel: Encapsulates the entire bus transport simulation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class BusSimulationModel(name: String = "BusSim", reps: Int = 1)
  extends Model(name, reps) {

  // Random loading/unloading time for buses
  val lTime = Uniform(1.0, 2.0)

  // Queue for passengers waiting for the bus
  val passengerQueue = WaitQueue("passengers", (100, 100))

  // Create bus with capacity 3
  val bus = new Bus("A", this, lTime, cap = 3)

  // Add bus and queue to the model
  addComponent(passengerQueue, bus)
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Main function: Run the bus simulation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
@main def runBusSimulation(): Unit = {

  println("=== Starting Bus Transport Simulation ===")

  // Create simulation model
  val sim = new BusSimulationModel()

  // Start the simulation
  sim.simulate()
  sim.waitFinished()           // Wait until all actors finish
  Model.shutdown()             // Shut down the simulation engine

  println("=== Simulation Finished ===")
}
