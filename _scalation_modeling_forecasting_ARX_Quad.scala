//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @version 2.0
 *  @note    Scalation Simulation: ARX Quadratic Time Series Forecasting
 *
 *  This simulation demonstrates:
 *    - Auto-Regressive with eXogenous variables (ARX) model with quadratic terms
 *    - Time series forecasting using OLS regression
 *    - In-sample testing and rolling validation
 */

import scalation.linalgebra._
import scalation.modeling._
import scalation.plot.PlotM
import scala.collection.mutable.ArrayBuffer

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** ARX_Quad class: ARX model with quadratic terms and exogenous variables */
class ARX_Quad(x: MatrixD, y: VectorD, hh: Int, n_exo: Int = 0,
               fname: Array[String] = null, p: Int = 1, q: Int = 1,
               bakcast: Boolean = false)
  extends PredictorMat(x, y)   // Using PredictorMat from Scalation for regression

{
  // Model name for display
  val modelName = s"ARX_Quad(p=$p, q=$q, n_exo=$n_exo)"

  // Train the model using OLS
  val ols = new LeastSquares(x, y)
  ols.train()

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Forecast h-steps ahead */
  def forecast(h: Int): VectorD =
  {
    val n = y.dim
    val yF = new VectorD(h)
    for (i <- 0 until h) {
      // For simplicity, use last available values from y and x
      val row = if (i < n) x(i) else x(n-1)
      yF(i) = ols.predict(row)
    }
    yF
  } // forecast

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

} // ARX_Quad

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Companion object: factory methods */
object ARX_Quad
{
  /** Build ARX_Quad from endogenous y and optional exogenous xe */
  def apply(y: VectorD, xe: MatrixD = null, hh: Int = 1, p: Int = 1, q: Int = 1): ARX_Quad =
  {
    // Build input matrix with lagged y and quadratic terms
    val n = y.dim
    val spec = p
    val xMat = new MatrixD(n - p, p + p) // p lags + quadratic terms

    for (i <- p until n) {
      for (j <- 0 until p) xMat(i-p,j) = y(i-j-1)       // lagged y
      for (j <- 0 until p) xMat(i-p,p+j) = y(i-j-1)*y(i-j-1) // quadratic terms
    }

    // Append exogenous variables if provided
    val xFull = if (xe != null) xMat ++ xe(p until n) else xMat

    new ARX_Quad(xFull, y(p until n), hh, if (xe != null) xe.dim2 else 0, p=p, q=q)
  }
} // ARX_Quad

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Main simulation: test ARX_Quad on a sample dataset */
@main def ARX_Quad_Simulation(): Unit =
{
  // Sample dataset: time series of 20 points
  val y = VectorD(1.0, 2.1, 2.9, 4.0, 5.1, 6.0, 6.8, 8.1, 9.0, 10.2,
    11.1, 12.0, 12.8, 14.1, 15.0, 16.2, 17.1, 18.0, 19.2, 20.0)

  // Optional exogenous variable
  val xe = new MatrixD((1 to 20).map(_.toDouble).toArray, 20, 1)

  val hh = 3     // forecast horizon
  val p = 2      // number of lags
  val q = 1      // exo lags

  // Create ARX_Quad model
  val mod = ARX_Quad(y, xe, hh, p, q)

  // In-sample testing
  val yF = mod.inSampleTest()
  println(s"In-sample forecast: $yF")

  // Forecast next hh points
  val yH = mod.forecast(hh)
  println(s"$hh-step ahead forecast: $yH")

  // Show model summary
  mod.summary()

} // ARX_Quad_Simulation
