//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Standalone Scala 2.12 Vehicle Simulation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scala.collection.mutable.Map

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Vehicle properties and defaults
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
object VehicleProps {
  val defaults: Map[String, Double] = Map(
    "rt"   -> 1.0,      // reaction time (s)
    "amax" -> 3.0,      // max acceleration (m/s^2)
    "bmax" -> -3.5,     // max deceleration (m/s^2)
    "v0"   -> 33.528,   // initial velocity (m/s)
    "vmax" -> 33.528,   // max velocity (m/s)
    "T"    -> 3.0,      // safe time headway (s)
    "s"    -> 5.0,      // minimum distance headway (m)
    "len"  -> 4.0,      // vehicle length (m)
    "del"  -> 4.0       // acceleration exponent
  )
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Vehicle class
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
abstract class Vehicle(val name: String, var props: Map[String, Double] = VehicleProps.defaults) {

  var disp: Double = 0.0       // current displacement along a path
  var velocity: Double = props("v0") // current velocity

  // getters for common properties
  def rt: Double   = props("rt")
  def amax: Double = props("amax")
  def bmax: Double = props("bmax")
  def vmax: Double = props("vmax")
  def T: Double    = props("T")
  def s: Double    = props("s")
  def len: Double  = props("len")
  def del: Double  = props("del")

  /** Set new vehicle properties */
  def setProps(newProps: Map[String, Double]): Unit = {
    props = newProps
  }

  /** Butcher's Method for numerical ODE solving (simplified) */
  def butcher(Ft: Double, ft: Double, ft_rt: Double, rt: Double): Double = {
    val k1 = ft_rt + 0.25 * (ft - ft_rt)
    val k3 = ft_rt + 0.50 * (ft - ft_rt)
    val k4 = ft_rt + 0.75 * (ft - ft_rt)
    val k5 = ft
    val k6 = ft // simplified last term
    Ft + (1.0 / 9.0) * (7*k1 + 32*k3 + 12*k4 + 32*k5 + 7*k6) * rt
  }

  /** Abstract act method to define vehicle behavior */
  def act(): Unit = println(s"$name.act method should be overridden")

}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Example of a concrete Vehicle subclass
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class Car(name: String) extends Vehicle(name) {
  override def act(): Unit = {
    // Example: simple displacement update
    disp += velocity
    println(f"$name moved to displacement $disp%.2f with velocity $velocity%.2f m/s")
  }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Simple test
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
object VehicleTest extends App {
  val car1 = new Car("Car1")
  car1.act()
  car1.act()
}
