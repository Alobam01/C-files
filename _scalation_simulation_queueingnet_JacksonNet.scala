import scalation.simulation._

class JacksonNet(p: MatrixD, r: VectorD, mu: VectorD, k: Array[Int] = null) extends Model {

  val m = p.dim1
  val ident = r *: Fac_LU.inverse(ident - p)()
  val k_ = if (k == null) Array.fill[Int](m)(1) else k
  val lambda = new VectorD(m)
  val rho = new VectorD(m)

  for (i <- 0 until m) {
    lambda(i) = r(i) * p(i, 0)
    rho(i) = lambda(i) / (mu(i) * k_(i).toDouble)
  }

  def pi_0(ro: Double, kk: Int): Double = {
    val sum = (for (i <- 0 until kk) yield ro ~^ i / fac(i)).sum
    1.0 / (sum + ro ~^ kk / (fac(kk) * (1.0 - ro)))
  }

  def nQueue(j: Int): Double = if (k_(j) > 1) nQueue_k(j) else nQueue_1(k_(j))

  def nQueue_1(j: Int): Double = {
    val ro = rho(j)
    ro ~^ 2 / (1.0 - ro)
  }

  def nQueue_k(j: Int): Double = {
    val ro = rho(j)
    pi_0(ro, k_(j)) * k_(j) ~^ k_(j) * ro ~^ (k_(j) + 1) / (fac(k_(j)) * (1.0 - ro) ~^ 2)
  }

  def report(): Unit = {
    for (j <- 0 until m) {
      val lQ = nQueue(j)
      val lS = lQ + lambda(j)
      val lT = lambda(j)
      println(s"\nResults for node $j:")
      println(f"lQ = $lQ%.2f\twQ = ${lQ / lambda(j)}%.2f")
      println(f"lS = $lS%.2f\twS = ${lS / lambda(j)}%.2f")
      println(f"lT = $lT%.2f\twT = ${lT / lambda(j)}%.2f")
    }
  }
}
