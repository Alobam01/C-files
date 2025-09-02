//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @version 2.0
 *  @note    Scalation Simulation: Auto-Regressive on lagged y (ARY) using OLS
 *
 *  This simulation demonstrates:
 *    - Time series forecasting using AR models
 *    - Endogenous lags (p)
 *    - Trend specification (constant, linear, quadratic)
 *    - In-sample testing and rolling validation
 */

import scalation.linalgebra._
import scalation.modeling._
import scalation.plot.PlotM

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** ARY class: Auto-Regressive on lagged y */
class ARY(x: MatrixD, y: VectorD, hh: Int, fname: Array[String] = null,
          p: Int = 1, spec: Int = 1)
  extends PredictorMat(x, y) // PredictorMat from Scalation

{
  val modelName = s"ARY(p=$p, spec=$spec)"

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Train the model using OLS */
  val ols = new LeastSquares(x, y)
  ols.train()

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Forecast h-steps ahead */
  def forecast(h: Int): VectorD =
  {
    val n = y.dim
    val yF = new VectorD(h)
    for (i <- 0 until h) {
      val row = if (i < n) x(i) else x(n-1)
      yF(i) = ols.predict(row)
    }
    yF
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** In-sample testing: predict all y using model */
  def inSampleTest(): VectorD =
  {
    val n = y.dim
    val yF = new VectorD(n)
    for (i <- 0 until n) yF(i) = ols.predict(x(i))
    yF
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Summary statistics */
  def summary(): Unit =
  {
    println(s"Model: $modelName")
    println("Coefficients: " + ols.coeff)
    println("Residuals: " + ols.resid)
    println("R^2: " + ols.R2)
  }
} // ARY

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Companion object: factory methods for ARY */
object ARY
{
  /** Build ARY model from time series y */
  def apply(y: VectorD, hh: Int = 1, p: Int = 1, spec: Int = 1): ARY =
  {
    val n = y.dim
    val xMat = new MatrixD(n - p, p + spec) // lags + trend

    for (i <- p until n) {
      // Add lagged y values
      for (j <- 0 until p) xMat(i-p, j) = y(i-j-1)
      // Add trend terms
      if (spec >= 1) xMat(i-p, p) = 1.0           // constant
      if (spec >= 2) xMat(i-p, p+1) = i.toDouble  // linear trend
      if (spec >= 3) xMat(i-p, p+2) = i*i.toDouble // quadratic trend
    }

    new ARY(xMat, y(p until n), hh, p=p, spec=spec)
  }
} // ARY

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Main simulation: test ARY on sample dataset */
@main def aRYTest(): Unit =
{
  // Sample dataset: simple time series
  val y = VectorD(1.0, 2.1, 2.9, 4.0, 5.1, 6.0, 6.8, 8.1, 9.0, 10.2)

  val hh = 3 // forecast horizon
  val p  = 2 // number of lags
  val spec = 2 // trend: 1=constant,2=linear,3=quadratic

  val mod = ARY(y, hh, p, spec)

  // In-sample test
  val yF = mod.inSampleTest()
  println(s"In-sample forecast: $yF")

  // Forecast next hh points
  val yH = mod.forecast(hh)
  println(s"$hh-step ahead forecast: $yH")

  // Model summary
  mod.summary()
} // aRYTest

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Example rolling validation (TnT) */
@main def aRYRollingValidation(): Unit =
{
  val y = VectorD(1.0, 2.1, 2.9, 4.0, 5.1, 6.0, 6.8, 8.1, 9.0, 10.2)
  val hh = 2
  val p  = 2
  val spec = 2

  val nTrain = 6
  val train = y(0 until nTrain)
  val test  = y(nTrain until y.dim)

  val mod = ARY(train, hh, p, spec)

  // Rolling validation: forecast each step in test
  val forecasts = new ArrayBuffer[Double]()
  for (i <- test.indices) {
    val yF = mod.forecast(1)
    forecasts += yF(0)
  }

  println(s"Rolling forecast: $forecasts")
} // aRYRollingValidation
