//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Standalone Scala 2.12 Source Simulation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scala.util.Random

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Variate trait for inter-arrival time generation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
trait Variate {
  def gen: Double
}

// Simple uniform distribution
case class Uniform(min: Double, max: Double) extends Variate {
  private val rand = new Random()
  def gen: Double = min + rand.nextDouble() * (max - min)
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// SimActor base class
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
abstract class SimActor(val name: String) {
  var mySource: Source = _
  var subtype: Int = 0

  def act(): Unit = println(s"$name acting (override in subclass)")
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Source class: periodically generates SimActors
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class Source(val name: String,
             val makeEntity: () => SimActor,
             val units: Int,
             val iArrivalTime: Variate) {

  private val generated = scala.collection.mutable.ListBuffer.empty[SimActor]

  /** Act method simulates the creation of entities */
  def act(): Unit = {
    println(s"Source $name starting generation of $units entities...")
    for (i <- 1 to units) {
      val actor = makeEntity()
      actor.mySource = this
      actor.subtype = 0
      generated += actor
      println(s"Generated entity $i: ${actor.name}")

      if (i < units) {
        val duration = iArrivalTime.gen
        println(f"Waiting $duration%.2f time units until next entity")
        Thread.sleep((duration * 1000).toLong) // simulate wait
      }
    }
    println(s"Source $name finished generating entities.")
  }

  /** Returns all generated entities */
  def getEntities: List[SimActor] = generated.toList
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Example usage
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
object SourceTest extends App {
  // Define a simple Car class
  case class Car(name: String) extends SimActor(name) {
    override def act(): Unit = println(s"$name driving...")
  }

  // Create a source that generates 5 cars with uniform inter-arrival time 2-5 seconds
  val carSource = new Source(
    name = "CarMaker",
    makeEntity = () => Car(s"Car${Random.nextInt(1000)}"),
    units = 5,
    iArrivalTime = Uniform(2, 5)
  )

  // Run the source
  carSource.act()

  // Access generated entities
  val cars = carSource.getEntities
  println("Generated cars:")
  cars.foreach(c => println(s"- ${c.name}"))
}
