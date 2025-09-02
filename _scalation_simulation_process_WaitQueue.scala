//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Standalone Scala 2.12 FCFS Wait Queue Simulation
// Author: Adapted from John Miller
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scala.collection.mutable.Queue
import scala.util.Random

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// SimActor represents an entity in the simulation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
case class SimActor(id: Int)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// WaitQueue class: First-Come, First-Serve Queue with optional capacity
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class WaitQueue(val name: String, val cap: Int = Int.MaxValue) {

  private val queue = new Queue[SimActor]()
  private var _barred: Int = 0
  private var totalWaitTime: Double = 0.0

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Check if queue is full
  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def isFull: Boolean = queue.size >= cap

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Return number of entities barred due to full queue
  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def barred: Int = _barred

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Entity attempts to join the queue
  // Returns true if joined, false if barred
  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def waitIn(actor: SimActor, waitTime: Double = 1.0): Boolean = {
    if (isFull) {
      _barred += 1
      false
    } else {
      queue.enqueue(actor)              // FCFS: enqueue at the end
      totalWaitTime += waitTime         // accumulate wait time
      true
    }
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Entity leaves the queue (FCFS: dequeues from front)
  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def leave(): Option[SimActor] = {
    if (queue.isEmpty) None
    else Some(queue.dequeue())
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Display queue statistics
  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  def report(): Unit = {
    println(s"\nQueue '$name' Report:")
    println(s"  Current size     = ${queue.size}")
    println(s"  Total barred     = $_barred")
    println(f"  Average wait time= ${if (queue.nonEmpty) totalWaitTime / queue.size else 0.0}%.2f")
    println(s"  Queue content    = ${queue.map(_.id).mkString("[", ", ", "]")}")
  }

} // WaitQueue

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Simulation Driver
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
object WaitQueueFCFSSim extends App {

  // Create a queue with capacity 5
  val queue = new WaitQueue("FCFS-Queue", cap = 5)

  // Generate 10 actors attempting to join the queue
  val actors = (1 to 10).map(i => SimActor(i))

  println("Simulation: Actors attempting to join FCFS queue")

  for (actor <- actors) {
    val joined = queue.waitIn(actor, waitTime = Random.nextDouble() * 2)
    println(s"Actor ${actor.id} ${if (joined) "joined" else "barred"} the queue")
  }

  // Remove 3 actors to simulate service
  println("\nProcessing 3 actors from queue...")
  for (_ <- 1 to 3) {
    queue.leave() match {
      case Some(actor) => println(s"Actor ${actor.id} left the queue")
      case None => println("Queue empty")
    }
  }

  // Final queue report
  queue.report()

}
