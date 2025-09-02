//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @version 2.1
 *  @note    Temporal trait: adds time-based functionality to identifiable objects
 */
package scalation.simulation

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Trait for objects that can be identified.
 *  @note This is a simple placeholder for demonstration purposes.
 */
trait Identifiable {
  def id: String
}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Trait that represents a temporal object, i.e., one that has an activation time
 *  and can be ordered in time.
 */
trait Temporal extends Identifiable {

  /** The activation time of this temporal object */
  var actTime: Double = 0.0

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Compare this temporal object with another based on activation time.
   *  @param other  the other temporal object
   *  @return       negative if this < other, 0 if equal, positive if this > other
   */
  def compare(other: Temporal): Int = actTime.compare(other.actTime)

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** String representation for debugging */
  override def toString: String = s"Temporal(id=$id, actTime=$actTime)"
}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Example implementation of a Temporal object for testing */
class Event(val id: String, val actTime: Double) extends Temporal

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Main simulation object to test Temporal trait */
object TemporalTest extends App {

  val e1 = new Event("E1", 5.0)
  val e2 = new Event("E2", 3.0)
  val e3 = new Event("E3", 10.0)

  val events: Array[Event] = Array(e1, e2, e3)

  println("Before sorting by actTime:")
  events.foreach(println)

  // Sort events by activation time
  scala.util.Sorting.quickSort(events)(Ordering.fromLessThan[Event]((a, b) => a.compare(b) < 0))

  println("\nAfter sorting by actTime:")
  events.foreach(println)
}
