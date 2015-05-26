/**
 * Created by junya_okumura on 2015/05/26.
 */
class Perceptron(samples: Seq[(Seq[Double], Boolean)], dim: Int) {
  val weights = new Array[Double](dim + 1)
  //input vector wn + w0
  val eta = 0.1



  for (step <- 1 to 10000) samples.foreach { case (in, out) => {
    predict(in) match {
      case true if !out =>
        for (k <- 0 until dim) weights(k + 1) -= eta * in(k)
        weights(0) -= eta * 1
      case false if out =>
        for (k <- 0 until dim) weights(k + 1) += eta * in(k)
        weights(0) += eta * 1
      case _ =>
    }
  }
  }

  def predict(in: Seq[Double]) = ((1.0 +: in) zip weights).map {
    case (i, w) => i * w
  }.sum >= 0


}

object main {
  def main(args: Array[String]) {

    val samples = Seq(
      (Seq(0.0, 0.0), false),
      (Seq(0.0, 1.0), true),
      (Seq(1.0, 0.0), true),
      (Seq(1.0, 1.0), true))
    val p = new Perceptron(samples,2)
    println(p.predict(Seq(0.0,0.0)))
    println(p.weights.length)
    println(p.weights.mkString(","))

  }
}
