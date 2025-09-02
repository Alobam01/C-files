//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Standalone Scala 2.12 Sink Simulation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scala.collection.mutable.ListBuffer

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Base Component class (simplified for standalone simulation)
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
abstract class Component(val name: String, val at: Array[Double])

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Recorder trait for tracking actor flow (simplified)
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
trait Recorder {
  private val records = ListBuffer.empty[(String, Double)]

  def tally(duration: Double): Unit = {
    println(f"Tallying duration: $duration%.2f")
  }

  def record(actor: SimActor, time: Double): Unit = {
    records += ((actor.name, time))
    println(f"Recording actor ${actor.name} at time $time%.2f")
  }

  def getRecords: List[(String, Double)] = records.toList
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Sink class: terminates entities
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class Sink(name: String, at: Array[Double])
  extends Component(name, at) with Recorder {

  println(s"Sink $name initialized at location: (${at.mkString(", ")})")

  /** Auxiliary constructor with default width/height */
  def this(name: String, xy: (Double, Double)) =
    this(name, Array(xy._1, xy._2, 20.0, 20.0))

  /** Simulate an actor leaving the system */
  def leave(actor: SimActor, currentTime: Double): Unit = {
    println(s"Actor ${actor.name} leaving sink $name at time $currentTime")
    tally(currentTime)             // tally the time actor spent
    record(actor, currentTime)     // record actor termination
  }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Companion object for Sink
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
object Sink {
  /** Create a single sink using coordinates */
  def apply(name: String, xy: (Int, Int)): Sink =
    new Sink(name, Array(xy._1.toDouble, xy._2.toDouble, 20.0, 20.0))

  /** Create a group of sinks using offsets */
  def group(xy: (Int, Int), snk: (String, (Int, Int))*): List[Sink] = {
    val sinkGroup = ListBuffer.empty[Sink]
    for (s <- snk) {
      sinkGroup += Sink(s._1, (xy._1 + s._2._1, xy._2 + s._2._2))
    }
    sinkGroup.toList
  }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Example usage
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
object SinkTest extends App {
  // Simple actor
  case class Car(name: String) extends SimActor(name)

  // Create a sink
  val mainSink = Sink("MainSink", (100, 200))

  // Simulate some actors leaving
  val cars = List(Car("Car1"), Car("Car2"), Car("Car3"))
  var currentTime = 0.0

  for (car <- cars) {
    currentTime += 1.5  // simulate passage of time
    mainSink.leave(car, currentTime)
  }

  println("Records in sink:")
  mainSink.getRecords.foreach { case (actorName, time) =>
    println(f"$actorName left at time $time%.2f")
  }
}
