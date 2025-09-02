//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Standalone VAR Simulation Example
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.modeling.forecast.Forecaster_RegV
import scalation.plot.Plot

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// VAR Class (simplified for standalone simulation)
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class VAR(x: MatrixD, y: MatrixD, hh: Int) extends Forecaster_RegV(x, y, hh) {

  // Simple training/test placeholder
  def trainNtest(): Unit = {
    println(s"Training VAR model with ${y.dim2} variables and horizon $hh")
    // In a full model, fit coefficients using OLS/GLS here
  }

  // Simple forecast function: just repeat last observed value
  def forecastAll(): MatrixD = {
    val yf = new MatrixD(y.dim, hh)
    for (i <- 0 until y.dim; h <- 0 until hh) yf(i, h) = y(i, y.dim2-1)
    yf
  }

  // Rolling validation placeholder
  def rollValidate(): Unit = {
    println(s"Running rolling validation for horizon $hh")
  }
}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Main simulation
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
@main def VARStandaloneSim(): Unit = {

  val n = 50        // number of time points
  val p = 3         // number of variables
  val hh = 5        // forecast horizon

  // Generate synthetic time series data: y_t = random walk
  val y = new MatrixD(n, p)
  for (i <- 0 until n; j <- 0 until p) {
    y(i, j) = if (i == 0) scala.util.Random.nextDouble() * 10
    else y(i-1, j) + scala.util.Random.nextGaussian()
  }

  val x = y         // for simplicity, use y as input features

  // Print synthetic data
  println("Synthetic Time Series Data:")
  println(y)

  // Create VAR model
  val varModel = new VAR(x, y, hh)

  // Train and test
  varModel.trainNtest()

  // Forecast
  val forecasts = varModel.forecastAll()
  println("Forecasts:")
  println(forecasts)

  // Rolling validation
  varModel.rollValidate()

  // Optional: simple plot of the first variable
  new Plot(VectorD.range(0, n), y(?, 0), null, s"Variable 0 Time Series", lines = true)
}
