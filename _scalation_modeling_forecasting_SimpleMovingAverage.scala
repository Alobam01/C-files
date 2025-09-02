// SimpleMovingAverageStandalone.scala
// Scala 2.12 standalone program that uses Scalation's VectorD for a Simple Moving Average forecast.
//
// Requires: scalation library JAR on the classpath (scalation_2.12 or similar).
//
// Compile:
//   scalac -cp /path/to/scalation.jar SimpleMovingAverageStandalone.scala
// Run:
//   scala  -cp ".:/path/to/scalation.jar" SimpleMovingAverageStandalone

import scalation.linalgebra.VectorD
import scala.math.max

object SimpleMovingAverageStandalone {

  /** Simple Moving Average model using Scalation VectorD.
   *  q = window size (number of prior values to average)
   *  hh = forecast horizon (how many steps ahead to forecast when asked)
   */
  class SimpleMovingAverage(val q: Int, val hh: Int = 1) {
    require(q >= 1, "q (window size) must be >= 1")
    require(hh >= 1, "hh (horizon) must be >= 1")

    /** compute mean of a subrange y[from] .. y[toExclusive-1] (handles bounds) */
    private def meanRange(y: VectorD, from: Int, toExclusive: Int): Double = {
      val f = max(0, from)
      val t = math.min(y.dim, toExclusive)
      val n = t - f
      if (n <= 0) 0.0 else y(f until t).sum / n.toDouble
    }

    /** 1-step-ahead prediction at time t (uses past q values y(t-q) .. y(t-1)) */
    def predict(t: Int, y: VectorD): Double = {
      val start = t - q
      meanRange(y, start, t) // average of last q values before t (if available)
    }

    /** Forecast h = 1..hh steps ahead starting from time t using recursive sliding window.
     *  The method appends predicted values to a temporary buffer so multi-step forecast
     *  uses prior predictions as needed (classic rolling forecast).
     *
     *  Returns a VectorD of length <= hh with forecasts for steps 1..hh.
     */
    def forecastFrom(t: Int, y: VectorD): VectorD = {
      val history = new scala.collection.mutable.ArrayBuffer[Double]()
      // copy data up to time point t (inclusive index t-1 is last observed)
      for (i <- 0 until y.dim) history += y(i)

      // If t < y.dim, we assume forecasts are being made from the end of the series.
      // We'll start forecasting from index 't' (i.e., next index after last observed index t-1).
      val startIndex = t

      val preds = new Array[Double](hh)
      for (h <- 1 to hh) {
        // determine indices of last q values in current history to average
        val lastIdx = history.size - 1
        val from = lastIdx - (q - 1)
        val toExclusive = history.size
        val avg = if (from < 0) {
          // not enough values yet, average what's available
          if (history.isEmpty) 0.0 else history.sum / history.size.toDouble
        } else {
          history.slice(from, toExclusive).sum / q.toDouble
        }
        preds(h - 1) = avg
        // append prediction (so subsequent horizons can use it)
        history += avg
      }
      VectorD(preds)
    }

    /** Forecast the next hh values from the end of the series (convenience) */
    def forecastLast(y: VectorD): VectorD = forecastFrom(y.dim, y)

    /** Produce forecasts for all time points for horizon h (in-sample rolling forecasts).
     *  For each t (starting at first index where q past values exist), produces t+h forecast.
     *  This is a simple in-sample predictor routine useful for diagnostics.
     */
    def forecastAll(h: Int, y: VectorD): VectorD = {
      require(h >= 1, "h must be >= 1")
      val out = new Array[Double](y.dim)
      for (t <- 0 until y.dim) {
        // predict value for time t+h using data up to t (i.e., next h-step forecast)
        val history = new scala.collection.mutable.ArrayBuffer[Double]()
        for (i <- 0 to t) history += y(i)
        // simulate rolling predictions until we reach step h
        var pred = 0.0
        for (step <- 1 to h) {
          val lastIdx = history.size - 1
          val from = lastIdx - (q - 1)
          val avg = if (from < 0) {
            if (history.isEmpty) 0.0 else history.sum / history.size.toDouble
          } else {
            history.slice(from, history.size).sum / q.toDouble
          }
          pred = avg
          history += pred
        }
        out(t) = pred
      }
      VectorD(out)
    }
  }

  // ------------------------
  // Demo / main
  // ------------------------
  def main(args: Array[String]): Unit = {
    // Sample dataset (e.g., lake levels, daily counts, etc.)
    val y = VectorD(112.0, 118.0, 132.0, 129.0, 121.0, 135.0, 148.0, 148.0, 136.0, 119.0)

    println(s"Original series (n = ${y.dim}):\n  $y\n")

    val q = 3      // window size: mean of last q values
    val hh = 5     // forecast horizon (how many steps ahead to forecast)
    val sma = new SimpleMovingAverage(q, hh)

    // 1-step prediction examples (predict value at time t using prior q)
    for (t <- 1 until y.dim) {
      val pred1 = sma.predict(t, y)
      println(f"1-step predict at t=$t%2d -> $pred1%8.4f (actual = ${y(t)}%6.2f)")
    }

    println("\nForecasting last hh steps (rolling, using prior forecasts for multi-step):")
    val forecasts = sma.forecastLast(y)
    println(s"Next $hh forecasts: ${forecasts.toString}")

    // Produce 2-step in-sample forecast vector for diagnostic
    val hCheck = 2
    val inSample2 = sma.forecastAll(hCheck, y)
    println(s"\nIn-sample $hCheck-step forecasts (for each t using data up to t):")
    println(inSample2)

    println("\nDone.")
  }
}
