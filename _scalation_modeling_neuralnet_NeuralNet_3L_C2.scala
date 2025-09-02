//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
// NeuralNet_3L_C2Standalone.scala
// A standalone Scalation program to train and test a 3-layer neural network classifier
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.linalgebra._
import scalation.modeling.classifying._
import scalation.util.banner

@main def NeuralNet_3L_C2Standalone(): Unit =

  banner("NeuralNet_3L_C2 Standalone Example")

// Sample dataset: 8 features, binary classification (0 or 1)
val x = MatrixD(
  (5.1, 3.5, 1.4, 0.2, 0.0, 0.1, 0.2, 0.1),
  (4.9, 3.0, 1.4, 0.2, 0.1, 0.0, 0.0, 0.0),
  (6.2, 3.4, 5.4, 2.3, 0.2, 0.1, 0.0, 0.0),
  (5.9, 3.0, 5.1, 1.8, 0.1, 0.0, 0.1, 0.2),
  (5.5, 2.3, 4.0, 1.3, 0.0, 0.1, 0.2, 0.1)
)

val y = VectorI(0, 0, 1, 1, 1)  // corresponding labels

val featureNames = Array("f1","f2","f3","f4","f5","f6","f7","f8")
val classNames = Array("No", "Yes")

// Hyperparameters
val hp = HyperParameter()
hp("eta") = 0.2         // learning rate
hp("cThresh") = 0.5     // classification threshold

// Create neural network classifier
val nn = new NeuralNet_3L_C2(x, y, featureNames, classNames, nz = 5, hparam = hp)

// Train the model
nn.train()
println("Training completed.")

// Test on the same dataset (for demo purposes)
val (predClasses, qof) = nn.test()
println(s"Predicted classes: $predClasses")
println(s"Quality of Fit: $qof")

end NeuralNet_3L_C2Standalone
