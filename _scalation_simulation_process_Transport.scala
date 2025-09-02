//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Standalone Scala 2.12 Transport Simulation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scala.collection.mutable.ArrayBuffer
import scala.math._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// VectorD simple implementation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
case class VectorD(x: Double, y: Double) {
  def +(v: VectorD): VectorD = VectorD(x + v.x, y + v.y)
  def -(v: VectorD): VectorD = VectorD(x - v.x, y - v.y)
  def *(s: Double): VectorD = VectorD(x * s, y * s)
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Simple QCurve class for paths (straight line)
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class QCurve(val start: VectorD, val end: VectorD) {
  var traj: Double = 0.0  // 0.0=start, 1.0=end

  def length: Double = sqrt(pow(end.x - start.x, 2) + pow(end.y - start.y, 2))

  def getFirst: Array[Double] = Array(start.x, start.y)

  def next(): VectorD = {
    // Move along curve based on traj
    val pos = start + (end - start) * traj
    traj = min(traj + 0.1, 1.0) // increment traj
    pos
  }

  def setLine(p1: VectorD, p2: VectorD): Unit = {
    traj = 0.0
  }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Component trait to represent endpoints
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
trait Component {
  def at: Array[Double]
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Transport class: moves vehicles from one Component to another along a QCurve
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class Transport(val name: String, val from: Component, val to: Component) {

  private val curve = new QCurve(
    VectorD(from.at(0), from.at(1)),
    VectorD(to.at(0), to.at(1))
  )

  private val onTransport = ArrayBuffer.empty[String]  // track vehicles on transport

  /** Return starting point of transport */
  def at: Array[Double] = curve.getFirst

  /** Move a vehicle along the transport */
  def move(vehicle: String): Unit = {
    println(s"Transport $name: moving vehicle $vehicle")
    onTransport += vehicle
    curve.traj = 0.0
    while (curve.traj < 1.0) {
      val pos = curve.next()
      println(f"$vehicle at x=${pos.x}%.2f, y=${pos.y}%.2f")
      // in a real simulation, here would be yield or sleep
    }
    println(s"$vehicle reached end of $name")
    onTransport -= vehicle
  }

  /** Jump the vehicle to the middle of the transport */
  def jump(vehicle: String): Unit = {
    curve.traj = 0.5
    val pos = curve.next()
    println(f"$vehicle jumped to x=${pos.x}%.2f, y=${pos.y}%.2f on $name")
  }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Example Component
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class Node(val x: Double, val y: Double) extends Component {
  def at: Array[Double] = Array(x, y)
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Simple Test
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
object TransportTest extends App {
  val start = new Node(0, 0)
  val end   = new Node(10, 5)
  val road  = new Transport("Road1", start, end)

  road.jump("Car1")   // Jump to middle
  road.move("Car1")   // Move along the path
}
