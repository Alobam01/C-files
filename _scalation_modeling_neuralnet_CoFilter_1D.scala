//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Standalone Simulation of a 1D Convolutional Filter using Scalation
 *  Adapted from John Miller’s Scalation library.
 *
 *  Compile: scalac -cp scalation.jar CoFilter_1D.scala
 *  Run:     scala -cp .:scalation.jar CoFilter_1DTest
 */

import scalation.linalgebra._
import scalation.mathstat._
import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CoFilter_1D` class provides a convolution filter (cofilter) for
 *  taking a weighted average over a window of an input vector.
 *  @param width  the width of the cofilter
 */
class CoFilter_1D(width: Int = 5) {
  private var vec: VectorD = RandomVecD(width, 2.0).gen  // random filter coefficients

  /** Update the parameters, i.e., the filter's vector. */
  def update(vec_ : VectorD): Unit = vec = vec_

  /** Get the coefficients (needed for forward/gradient computation). */
  def coef: VectorD = vec
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CoFilter_1D` companion object provides convolution and pooling operators.
 */
object CoFilter_1D {

  //--- Convolution operators ------------------------------------------------
  def conv(c: VectorD, x: VectorD): VectorD = c *+ x
  def conv(c: VectorD, x: MatrixD): MatrixD = x.mmap(c *+ _)

  def convs(c: VectorD, x: VectorD): VectorD = c *~+ x
  def convs(c: VectorD, x: MatrixD): MatrixD = x.mmap(c *~+ _)

  def convf(c: VectorD, x: VectorD): VectorD = c *++ x
  def convf(c: VectorD, x: MatrixD): MatrixD = x.mmap(c *++ _)

  //--- Pooling operators ----------------------------------------------------
  def pool(x: VectorD, s: Int = 2): VectorD = {
    val p = new VectorD(x.dim / s)
    for (j <- p.indices) {
      val jj = s * j
      p(j) = x(jj until jj+s).max
    }
    p
  }

  def pool(x: MatrixD, s: Int): MatrixD = x.mmap(pool(_, s))

  def pool_a(x: VectorD, s: Int = 2): VectorD = {
    val p = new VectorD(x.dim / s)
    for (j <- p.indices) {
      val jj = s * j
      p(j) = x(jj until jj+s).mean
    }
    p
  }

  def pool_a(x: MatrixD, s: Int): MatrixD = x.mmap(pool_a(_, s))
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Test `CoFilter_1D` with simple convolution and pooling examples.
 */
object CoFilter_1DTest extends App {
  import CoFilter_1D._

  banner("Test 1: Simple Convolution + Pooling")
  val x = VectorD(1, 2, 3, 4, 5)
  val c = VectorD(0.5, 1.0, 0.5)

  val φ = conv(c, x)
  val z = pool(φ, 2)

  println(s"input       x = $x")
  println(s"filter      c = $c")
  println(s"feature map φ = $φ")
  println(s"pooled      z = $z")

  banner("Test 2: Same Convolution")
  println(s"y = ${convs(c, x)}")

  banner("Test 3: Full Convolution")
  println(s"y = ${convf(c, x)}")
}
