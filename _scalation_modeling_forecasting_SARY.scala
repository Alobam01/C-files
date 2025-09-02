//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// SARYSimulation.scala
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.linalgebra._
import scalation.modeling.forecasting._
import scalation.random._
import scalation.simulation.process._
import scalation.plot._

/** A simulation that applies the SARY forecasting model
 *  step-by-step as time advances.
 */
@main def runSARYSimulation(): Unit =

// Example dataset (could be LakeLevels, COVID, etc.)
val y = VectorD(10, 11, 13, 12, 14, 15, 16, 18, 17, 19)

val hh = 3
val mod = SARY(y, hh)          // build SARY model from data
mod.trainNtest_x ()()          // train and test once at start

// Simulation setup
val sim = new Model("SARY-Forecasting-Simulation")

// Forecasting entity
class Forecaster(name: String) extends SimActor(name, sim):
def act(): Unit =
  for t <- 1 to 20 do
val y_next = mod.forecastAt(t, 1)   // 1-step forecast
println(s"Time $t => Forecast = $y_next")
yieldTo(sim, 1.0)                   // advance simulation time
end for
  end act
    end Forecaster

// Add the forecasting actor
new Forecaster("SARY_Forecaster").schedule(0.0)

// Run the simulation
sim.simulate()
end runSARYSimulation
