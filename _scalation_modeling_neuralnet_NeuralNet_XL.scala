//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Mar 16 15:13:38 EDT 2018
 *  @note    Standalone Scalation Simulation: Neural Network with 4 Layers
 */
import scalation.linalgebra._
import scalation.modeling.hyperparam.HyperParameter
import scalation.modeling.optimization.Optimizer
import scalation.modeling.neuralnet._
import scalation.mathstat._
import scalation.util.banner

@main def NeuralNet_XL_Simulation(): Unit =

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Example training data (12 samples, 3 features)
val x = MatrixD((12, 3),
  1.0, 0.2, 0.3,
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
  1.0, 0.5, 0.7
)

// Target output: simple sigmoid of weighted sum
val y = x.map(v => VectorD(sigmoid(v.sum)))

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Set hyperparameters
Optimizer.hp("eta") = 0.3     // learning rate
Optimizer.hp("bSize") = 4.0   // batch size

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// Create NeuralNet_XL model
val nn = new NeuralNet_XL(x, y,
  f = Array(f_sigmoid, f_sigmoid, f_id)) // 3 layers: input-hidden-output

banner("Training NeuralNet_XL")
nn.trainNtest()()                      // train and test

banner("Results")
println("Predictions:")
println(nn.predict(x))                 // predict on training data

banner("Cross-Validation")
nn.crossValidate()                     // run cross-validation

end NeuralNet_XL_Simulation
