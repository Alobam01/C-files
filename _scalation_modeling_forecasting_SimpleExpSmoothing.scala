//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Standalone Simulation: Simple Exponential Smoothing (SES)
 *  Based on Scalation forecasting framework.
 *  Compatible with Scala 2.12
 */

import scalation.linalgebra._
import scalation.modeling.forecasting._
import scalation.plot._

object SimpleExpSmoothingStandalone:

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** SES Forecaster implementation using Scalation
 */
class SimpleExpSmoothing(y: VectorD, hh: Int, alpha: Double = 0.5)
  extends Forecaster(y, hh, null, null, false):

private val s = new VectorD(y.dim)      // smoothed values
modelName = "SimpleExpSmoothing"

// Train = compute smoothed values
override def train(x_null: MatrixD, y_ : VectorD): Unit =
  s(0) = y_(0)
for t <- 1 until y_.dim do
  s(t) = alpha * y_(t-1) + (1 - alpha) * s(t-1)
end for
  end train

// Forecast future values (constant = last smoothed)
override def forecast(h: Int = 1): VectorD =
  VectorD.fill(h)(s.last)

// Return model parameters
override def parameter: VectorD = VectorD(alpha)

// Predict value at time t
override def predict(t: Int, y_ : VectorD): Double =
  s(math.min(t, s.dim - 1))
end SimpleExpSmoothing

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Standalone demo
 */
@main def runSES(): Unit =
val y = VectorD(112, 118, 132, 129, 121, 135, 148, 148, 136, 119)  // sample time series
val hh = 5
val alpha = 0.5

val model = new SimpleExpSmoothing(y, hh, alpha)
banner(s"Training SES with alpha = $alpha")
model.train(null, y)

val forecasts = model.forecast(hh)

println(s"Original Data   = $y")
println(s"Smoothed Values = ${model.predict(0, y)} ... last = ${forecasts(0)}")
println(s"$hh-step Forecasts = $forecasts")

// Optional plot
new Plot(y, null, forecasts, s"SES Forecast (alpha=$alpha)")
