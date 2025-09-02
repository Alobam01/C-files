//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @version 2.1
 *  @note    M/M/1 Queue Simulation Model using tableau package (Scalation-style)
 */
package scalation.simulation.tableau

import scalation.random.Known
import scalation.random.RandomSeeds.N_STREAMS
import scalation.simulation.tableau.Model

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Main function to simulate a simple M/M/1 queue where service is provided by one teller.
 *  > runMain scalation.simulation.tableau.RunQueueMM1
 */
@main def RunQueueMM1 (): Unit = {

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Inter-arrival times for customers */
  val iArrivalArr = Array[Double](6, 3, 5, 4, 3, 8, 5, 7, 9, 6)

  /** Service times for customers */
  val serviceArr  = Array[Double](5, 6, 4, 3, 5, 7, 2, 4, 5, 8)

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Random number stream (0 to 999) */
  val stream = 0

  /** Maximum number of customers to simulate (stopping rule) */
  val maxCusts = 10

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Random variate generators using Known distribution (repeats given sequence) */
  val iArrivalRV = Known(iArrivalArr, stream)                       // inter-arrival time RV
  val serviceRV  = Known(serviceArr, (stream + 1) % N_STREAMS)      // service time RV

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Create the M/M/1 queue simulation model */
  val mm1 = new Model("Queue_MM1", maxCusts, Array(iArrivalRV, serviceRV))

  // Run the simulation
  mm1.simulate()
  mm1.report()     // detailed report
  mm1.summary()    // summary statistics

  // Compute server occupancy from timeline
  Model.occupancy(mm1.timeLine())

  println("\nSimulation completed successfully.")
}
