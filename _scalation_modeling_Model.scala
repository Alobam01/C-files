//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Standalone Simulation Example
 *  Based on Scalation’s Model framework (simplified).
 *  Works with Scala 2.12
 */
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

import scalation.linalgebra._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Model` trait provides a common framework for all models and serves as
 *  base trait for Classifier, Forecaster, Predictor, etc.
 */
trait Model {

  /** The name for the model (or modeling technique). */
  var modelName: String = "BaseModel"

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Order vectors y_ and yp_ according to ascending order of y_
   *  Useful for graphical comparison of true vs predicted values.
   */
  def orderByY (y_ : VectorD, yp_ : VectorD): (VectorD, VectorD) = {
    val idx = y_.argSort()
    (y_(idx), yp_(idx))
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Order matrices y_ and yp_ column by column according to ascending order of y_
   */
  def orderByYY (y_ : MatrixD, yp_ : MatrixD): (MatrixD, MatrixD) = {
    val idx = y_(?, 0).argSort()     // order by first column
    (y_(idx), yp_(idx))
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return the used data matrix x.
   *  Override in derived models if x is expanded/transformed.
   */
  def getX: MatrixD = null

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return the used response vector y.
   */
  def getY: VectorD = null

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return the used response matrix y (multi-output).
   */
  def getYY: MatrixD = null

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Return the feature/variable names.
   */
  def getFname: Array [String] = Array.empty

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Dummy train method (override in concrete models).
   */
  def train (x: MatrixD, y: VectorD): Unit = {
    println(s"Training $modelName on dataset (${x.dim1} x ${x.dim2}) ...")
  }

  //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  /** Dummy test method (override in concrete models).
   */
  def test (x: MatrixD, y: VectorD): (VectorD, VectorD) = {
    println(s"Testing $modelName ...")
    val yp = new VectorD(y.dim)       // predict zeros by default
    (y, yp)
  }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Example: NullModel extending Model
 */
class NullModel (y: VectorD) extends Model {
  override var modelName = "NullModel"
  override def getY: VectorD = y

  override def train (x: MatrixD, y: VectorD): Unit = {
    println(s"NullModel ignores training data")
  }

  override def test (x: MatrixD, y: VectorD): (VectorD, VectorD) = {
    val yp = new VectorD(y.dim)   // always predict zeros
    (y, yp)
  }
}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Standalone main to run the simulation.
 */
object ModelSimulation extends App {
  // Example dataset
  val x = new MatrixD((5, 2), 1, 2,
    2, 3,
    3, 4,
    4, 5,
    5, 6)
  val y = VectorD(2, 3, 5, 7, 11)

  // Instantiate a simple model
  val model = new NullModel(y)

  // Train and test
  model.train(x, y)
  val (yy, yp) = model.test(x, y)

  println(s"True values:     $yy")
  println(s"Predicted values: $yp")

  // Ordering example
  val (yo, ypo) = model.orderByY(y, yp)
  println(s"Ordered by Y: $yo vs $ypo")
}
