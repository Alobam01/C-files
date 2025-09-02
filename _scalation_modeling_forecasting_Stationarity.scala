//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Standalone Scalation Simulation:
 *  Demonstrates time-series simulation + Unit Root Test (ADF).
 *  Scala 2.12 compatible.
 *  Requires: scalation dependencies on classpath
 */
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.linalgebra._
import scalation.random.RandomVecD
import scalation.analytics.forecaster.UnitRoot
import scalation.analytics.forecaster.StationaryTest
import scalation.plot.Plot

object UnitRootStandalone extends App {

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // 1. Generate synthetic AR(1) time series: y_t = 0.8 * y_(t-1) + e_t
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  val n       = 200                           // number of observations
  val phi     = 0.8                           // AR(1) coefficient
  val noise   = RandomVecD(n, () => scala.util.Random.nextGaussian()) // white noise
  val series  = new VectorD(n)

  series(0) = noise(0)
  for (t <- 1 until n) {
    series(t) = phi * series(t-1) + noise(t)
  }

  println("First 10 observations of simulated series:")
  println(series(0 until 10))

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // 2. Run Augmented Dickey-Fuller (ADF) Test
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  val adfStat = UnitRoot.adfTest(series)
  println(s"\nADF Test Statistic = $adfStat")

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // 3. Optionally run KPSS Test (stationarity test)
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  val kpssStat = StationaryTest.kpssTest(series)
  println(s"KPSS Test Statistic = $kpssStat")

  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  // 4. Plot the simulated time series
  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  new Plot(null, series, null, "Simulated AR(1) Time Series", lines = true)
}
