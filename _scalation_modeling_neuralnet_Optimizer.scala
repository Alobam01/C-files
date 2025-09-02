//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, adapted
 *  @version 1.0
 *  @see     LICENSE (MIT style)
 *
 *  @note    Standalone Scalation simulation for SGD optimization on a neural network
 */

import scalation.linalgebra._
import scalation.util._
import scala.util.Random

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Hyper-parameters for optimization
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
object OptimizerHP extends HyperParameter:
this += ("eta", 0.1)        // learning rate
this += ("bSize", 5)        // batch size
this += ("maxEpochs", 50)   // max number of epochs
this += ("lambda", 0.01)    // regularization

import OptimizerHP._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Neural network parameters for one layer
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
case class NetParam(var W: MatrixD, var b: VectorD)

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Simple Optimizer trait
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
trait Optimizer:

def optimize(x: MatrixD, y: MatrixD, b: NetParam, eta: Double): Double =
val m = x.dim1
for epoch <- 0 until maxEpochs:
var loss = 0.0
for i <- 0 until m:
val xi = x(i, ?).t
val yi = y(i, ?).t
val z = b.W.t * xi + b.b
val pred = z.map(v => 1.0 / (1.0 + math.exp(-v))) // sigmoid
val error = pred - yi
b.W -= (xi * error.t) * eta
b.b -= error * eta
loss += (error dot error) / 2.0
if epoch % 10 == 0 then println(s"Epoch $epoch, Loss = $loss")
loss
end optimize

end Optimizer

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Standalone simulation
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
@main def runOptimizerSim (): Unit =

  println("Standalone Scalation Neural Network Simulation - SGD Optimizer")

// Generate synthetic data: y = x1 + x2 + noise
val m = 50       // number of samples
val n = 2        // number of features
val x = new MatrixD(m, n)
val y = new MatrixD(m, 1)
val rand = new Random(42)

for i <- 0 until m do
val xi1 = rand.nextDouble() * 10
val xi2 = rand.nextDouble() * 10
x(i, 0) = xi1
x(i, 1) = xi2
y(i, 0) = xi1 + xi2 + rand.nextGaussian() * 0.5

// Initialize network parameters (weights + bias)
val W = new MatrixD(n, 1) { for i <- indices; j <- range2 do this(i,j)=rand.nextDouble() }
val b = new VectorD(1, 0.0)
val netParam = NetParam(W, b)

// Run optimizer
val optimizer = new Optimizer {}     // anonymous class
val finalLoss = optimizer.optimize(x, y, netParam, eta)

println(s"Final Loss after training: $finalLoss")
println(s"Trained weights: ${netParam.W}")
println(s"Trained bias: ${netParam.b}")

end runOptimizerSim
