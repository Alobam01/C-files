//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Simple Bank Simulation in Scalation (Standalone, Scala 2.12)
 *  Demonstrates how to use scalation.simulation.process._ to model arrivals,
 *  waiting, and service at a bank with one teller.
 *
 *  Run with Scala 2.12 and Scalation 1.7+ library.
 */

import scalation.simulation.process._
import scalation.random.{Exponential, Uniform}

object BankSim extends App
{
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Define the simulation model
  val bank = new Model ("Bank")

  // Arrival and service time distributions
  val iArrivalRV = Exponential(6.0)   // mean inter-arrival = 6 minutes
  val serviceRV  = Uniform(4.0, 8.0)  // service time between 4 and 8 minutes

  // Teller is a resource with capacity = 1
  val teller = new Resource ("Teller", bank, 1)

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Define the customer actor
  class Customer (name: String, val at: Double)
    extends SimActor (name, bank)
  {
    def act (): Unit =
    {
      // arrive at the bank
      println (s"$me arrives at $clock")

      // request teller
      teller.acquire ()
      println (s"$me begins service at $clock")

      // undergo service
      hold (serviceRV.gen)

      // release teller
      teller.release ()
      println (s"$me departs at $clock")
    }
  }

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Define the source of customers
  case class CustomerSource (name: String, nArrivals: Int) extends Source (name, bank, () => new Customer ("c", clock), iArrivalRV, nArrivals)

  // Generate 10 customers
  CustomerSource ("CustomerSource", 10)

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // Run the simulation
  bank.simulate ()
  bank.waitFinished ()
  Model.shutdown ()
}
