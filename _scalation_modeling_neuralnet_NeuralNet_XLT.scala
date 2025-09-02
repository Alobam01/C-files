//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Standalone NeuralNet_XLT Simulation using Scalation
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.linalgebra._
import scalation.modeling.hyperparam.HyperParameter
import scalation.modeling.neuralnet._
import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Hyper-parameters
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
val hp = HyperParameter()
hp("eta") = 0.02  // learning rate

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Synthetic dataset: 10 samples, 3 input features, 2 output targets
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
val x = MatrixD((10,3), 1.0, 2.0, 3.0,
  2.0, 3.0, 4.0,
  3.0, 4.0, 5.0,
  4.0, 5.0, 6.0,
  5.0, 6.0, 7.0,
  6.0, 7.0, 8.0,
  7.0, 8.0, 9.0,
  8.0, 9.0,10.0,
  9.0,10.0,11.0,
  10.0,11.0,12.0)

val y = MatrixD((10,2), 1.0, 0.0,
  0.0, 1.0,
  1.0, 0.0,
  0.0, 1.0,
  1.0, 0.0,
  0.0, 1.0,
  1.0, 0.0,
  0.0, 1.0,
  1.0, 0.0,
  0.0, 1.0)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Define hidden layer sizes
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
val nz = Array(4, 3)  // two hidden layers: 4 and 3 nodes

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Create Neural Network without transfer learning
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
val nn = new NeuralNet_XLT(x, y, nz=nz, hparam=hp)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Train and test the network
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
@main def runNeuralNetSimulation(): Unit =
{
  banner("Standalone NeuralNet_XLT Simulation")
  nn.trainNtest()
  println("Predictions on training data:")
  println(nn.predict(x))
}
