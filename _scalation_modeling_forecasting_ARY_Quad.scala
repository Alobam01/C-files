//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @version 2.0
 *  @date    2025-08-29
 *  @note    Scalation Simulation: Auto-Regressive Quadratic (ARY_Quad) using OLS
 *
 *  Features:
 *    - Quadratic regression on lagged y
 *    - Multi-horizon direct forecasting
 *    - Trend specification (constant, linear, quadratic)
 *    - In-sample testing and rolling validation
 */

import scalation.linalgebra._
import scalation.modeling._
import scala.collection.mutable.ArrayBuffer

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** ARY_Quad class: Auto-Regressive with quadratic terms for multi-horizon forecasting */
class ARY_Quad(x: MatrixD, y: VectorD, hh: Int, p: Int = 1, spec: Int = 1, power: Double = 2.0)
{
  val modelName = s"ARY_Quad(p=$p, spec=$spec, power=$power)"
  val n = x.dim1
  val reg = Array.fill(hh)(new LeastSquares(x, y))  // separate regression per horizon

  // Train all hh regressions
  for (h <- 0 until hh) reg(h).train()

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Forge a feature vector for quadratic AR */
  def forge(t: Int, h: Int, yf: VectorD = null): VectorD =
  {
    var xVec = x(t).copy
    // Quadratic terms
    xVec = xVec ++ (xVec.map(v => math.pow(v, power)))
    xVec
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Predict 1-step ahead (direct method) */
  def predict(t: Int, h: Int): Double =
  {
    require(h >= 0 && h < hh, s"h = $h out of range 0..${hh-1}")
    val xVec = forge(t, h)
    reg(h).predict(xVec)
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Forecast h-steps ahead from time t */
  def forecast(t: Int): VectorD =
  {
    val yF = new VectorD(hh)
    for (h <- 0 until hh) yF(h) = predict(t, h)
    yF
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** In-sample testing */
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

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Rolling validation */
  def rollingValidation(trainEnd: Int, yFull: VectorD): VectorD =
  {
    val test = yFull(trainEnd until yFull.dim)
    val forecasts = new ArrayBuffer[Double]()
    for (i <- test.indices) {
      val t = math.min(i, trainEnd-1)
      forecasts += forecast(t)(0)
    }
    VectorD(forecasts.toArray)
  }
} // ARY_Quad

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Companion object: factory for ARY_Quad */
object ARY_Quad
{
  def apply(y: VectorD, hh: Int = 1, p: Int = 1, spec: Int = 1, power: Double = 2.0): ARY_Quad =
  {
    val n = y.dim
    val xMat = new MatrixD(n - p, p + spec)

    for (i <- p until n) {
      for (j <- 0 until p) xMat(i-p, j) = y(i-j-1)         // lagged y
      if (spec >= 1) xMat(i-p, p) = 1.0                     // constant
      if (spec >= 2) xMat(i-p, p+1) = i.toDouble            // linear
      if (spec >= 3) xMat(i-p, p+2) = i*i.toDouble          // quadratic
    }

    new ARY_Quad(xMat, y(p until n), hh, p, spec, power)
  }
} // ARY_Quad

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Main simulation: test ARY_Quad */
@main def aRY_QuadTest(): Unit =
{
  val y = VectorD(1.0, 2.1, 2.9, 4.0, 5.1, 6.0, 6.8, 8.1, 9.0, 10.2)
  val hh = 3
  val p = 2
  val spec = 2
  val power = 2.0

  val mod = ARY_Quad(y, hh, p, spec, power)

  val yf = mod.inSampleTest()
  println("In-sample forecasts:")
  println(yf)

  mod.summary()
} // aRY_QuadTest

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Rolling validation example */
@main def aRY_QuadRolling(): Unit =
{
  val y = VectorD(1.0, 2.1, 2.9, 4.0, 5.1, 6.0, 6.8, 8.1, 9.0, 10.2)
  val hh = 2
  val p = 2
  val spec = 2
  val power = 2.0
  val nTrain = 6

  val train = y(0 until nTrain)
  val test = y(nTrain until y.dim)

  val mod = ARY_Quad(train, hh, p, spec, power)

  val forecasts = mod.rollingValidation(nTrain, y)
  println(s"Rolling forecast: $forecasts")
} // aRY_QuadRolling
