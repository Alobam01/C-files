//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @version 2.0
 *  @date    2025-08-29
 *  @note    Scalation Simulation: Direct Auto-Regressive on lagged y (ARY_D) using OLS
 *
 *  Features:
 *    - Multi-horizon direct forecasting (h = 1 to hh)
 *    - Endogenous lags (p)
 *    - Trend specification (constant, linear, quadratic)
 *    - In-sample testing
 *    - Rolling validation example
 */

import scalation.linalgebra._
import scalation.modeling._
import scala.collection.mutable.ArrayBuffer

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** ARY_D class: Direct Auto-Regressive for multi-horizon forecasting */
class ARY_D(x: MatrixD, y: MatrixD, hh: Int, p: Int = 1, spec: Int = 1)
{
  val modelName = s"ARY_D(p=$p, spec=$spec)"
  val n = y.dim1
  val reg = Array.fill(hh)(new LeastSquares(x, y.col(hh-1)))  // One OLS per horizon

  // Train all hh regressions (direct multi-horizon)
  for (h <- 0 until hh) reg(h).train()

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Predict 1-step ahead (or t-th step in horizon h) */
  def predict(t: Int, h: Int): Double =
  {
    require(h >= 0 && h < hh, s"h = $h out of range 0..${hh-1}")
    reg(h).predict(x(t))
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Forecast h-steps ahead from time t (direct method) */
  def forecast(t: Int): VectorD =
  {
    val yF = new VectorD(hh)
    for (h <- 0 until hh) yF(h) = predict(t, h)
    yF
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** In-sample testing: forecast all points in y */
  def inSampleTest(): MatrixD =
  {
    val yf = new MatrixD(n, hh)
    for (t <- 0 until n) {
      val f = forecast(t)
      for (h <- 0 until hh) yf(t, h) = f(h)
    }
    yf
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Print model summary */
  def summary(): Unit =
  {
    println(s"Model: $modelName")
    for (h <- 0 until hh) println(s"Horizon ${h+1} Coefficients: ${reg(h).coeff}")
  }
} // ARY_D

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Companion object for ARY_D: factory methods */
object ARY_D
{
  // Build ARY_D object from time series y
  def apply(y: VectorD, hh: Int = 1, p: Int = 1, spec: Int = 1): ARY_D =
  {
    val n = y.dim
    val xMat = new MatrixD(n - p, p + spec)  // lagged values + trend

    for (i <- p until n) {
      // Lagged values
      for (j <- 0 until p) xMat(i-p, j) = y(i-j-1)
      // Trend terms
      if (spec >= 1) xMat(i-p, p) = 1.0           // constant
      if (spec >= 2) xMat(i-p, p+1) = i.toDouble  // linear
      if (spec >= 3) xMat(i-p, p+2) = i*i.toDouble // quadratic
    }

    // Build response matrix: one column per horizon (direct method)
    val yMat = new MatrixD(n - p, hh)
    for (t <- 0 until n - p; h <- 0 until hh) {
      if (t + h < y.dim) yMat(t, h) = y(t + h)
      else yMat(t, h) = y(n-1) // repeat last value if horizon exceeds
    }

    new ARY_D(xMat, yMat, hh, p, spec)
  }
} // ARY_D

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Main simulation: test ARY_D */
@main def aRY_DTest(): Unit =
{
  // Sample time series
  val y = VectorD(1.0, 2.1, 2.9, 4.0, 5.1, 6.0, 6.8, 8.1, 9.0, 10.2)
  val hh = 3   // forecast horizon
  val p  = 2   // number of lags
  val spec = 2 // trend: 1=constant,2=linear,3=quadratic

  val mod = ARY_D(y, hh, p, spec)

  // In-sample test
  val yf = mod.inSampleTest()
  println("In-sample forecasts:")
  println(yf)

  // Model summary
  mod.summary()
} // aRY_DTest

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Rolling validation example */
@main def aRY_DRollingValidation(): Unit =
{
  val y = VectorD(1.0, 2.1, 2.9, 4.0, 5.1, 6.0, 6.8, 8.1, 9.0, 10.2)
  val hh = 2
  val p  = 2
  val spec = 2
  val nTrain = 6

  val train = y(0 until nTrain)
  val test  = y(nTrain until y.dim)

  val mod = ARY_D(train, hh, p, spec)

  val forecasts = new ArrayBuffer[Double]()
  for (i <- test.indices) {
    val yF = mod.forecast(math.min(i, train.dim-1))
    forecasts += yF(0)
  }

  println(s"Rolling forecast: $forecasts")
} // aRY_DRollingValidation
