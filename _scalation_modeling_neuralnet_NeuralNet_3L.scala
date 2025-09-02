//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// NeuralNet3LSim.scala: Standalone Scalation Simulation for a 3-layer Neural Network
// Author: John Miller (adapted)
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.linalgebra._
import scalation.modeling.optimization._
import scalation.plot._

// Define a small training dataset
val x = MatrixD ((12, 3), 1.0, 0.2, 0.3,
  1.0, 0.2, 0.5,
  1.0, 0.2, 0.7,
  1.0, 0.3, 0.3,
  1.0, 0.3, 0.5,
  1.0, 0.3, 0.7,
  1.0, 0.4, 0.3,
  1.0, 0.4, 0.5,
  1.0, 0.4, 0.7,
  1.0, 0.5, 0.3,
  1.0, 0.5, 0.5,
  1.0, 0.5, 0.7)

val y = x.map(x_i => 1.0 / (1.0 + math.exp(-VectorD(2.0, 2.0, 2.0) dot x_i)))

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Create the NeuralNet_3L model
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

val nn = new NeuralNet_3L(x, y, nz = 4) // 4 hidden nodes

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Train and test the network
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

@main def NeuralNet3LSim(): Unit =
  println("=== Training NeuralNet_3L ===")
nn.train()                   // train with default hyperparameters
println("Training complete.")

println("\n=== Testing NeuralNet_3L ===")
val (yPred, _) = nn.test()
println(s"Predicted outputs: \n$yPred")

println("\n=== Plot Loss ===")
nn.opti.plotLoss("NeuralNet_3L")  // plot loss vs epochs
