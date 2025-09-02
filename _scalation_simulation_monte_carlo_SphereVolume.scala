//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Standalone Scalation Simulation: Monte Carlo Estimation of Unit Sphere Volume
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.random.Random

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Object containing the Monte Carlo Sphere Volume Simulation
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
object MonteCarloSphereSimulation {

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Main function to run the Monte Carlo simulation
   *  Estimates the volume of a unit sphere in 3D
   */
  def main(args: Array[String]): Unit = {

    println("=== Monte Carlo Simulation: Volume of a Unit Sphere ===\n")

    val nSamples = 1000000                       // Number of random points to generate
    val rand = Random()                           // Random number generator
    var countInside = 0                           // Counter for points inside the sphere

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Function to check whether a 3D point (x, y, z) lies inside the unit sphere
     *  @param x  x-coordinate
     *  @param y  y-coordinate
     *  @param z  z-coordinate
     *  @return true if the point is inside the unit sphere
     */
    def inSphere(x: Double, y: Double, z: Double): Boolean =
      x*x + y*y + z*z <= 1.0

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Generate random points in the cube [-1, 1]^3 and count points inside the sphere
    for (_ <- 0 until nSamples) {
      val x = 2.0 * rand.gen - 1.0             // Random x in [-1, 1]
      val y = 2.0 * rand.gen - 1.0             // Random y in [-1, 1]
      val z = 2.0 * rand.gen - 1.0             // Random z in [-1, 1]
      if (inSphere(x, y, z)) countInside += 1
    }

    // Monte Carlo estimation of the unit sphere volume
    val volumeEstimate = 8.0 * countInside.toDouble / nSamples.toDouble  // 8 = cube volume

    // Display results
    println(s"Estimated Volume of Unit Sphere = $volumeEstimate")
    println(f"Exact Volume of Unit Sphere      = ${4.0/3.0 * math.Pi}%.6f")
    println("\n=== Simulation Finished ===")
  }
}
