/**
 * Created by junya_okumura on 2015/05/26.
 */

//import breeze.linalg._

import Math._

// Dataクラスは１件のサンプルデータ
case class Data(point: Seq[Double], answer: Any = 0)

// k最近傍本体
class KNN(k: Int, samples: Seq[Data], distance: (Data, Data) => Double) {
  def predict(target: Data): Any = {
    val knears = samples.sortWith((t1, t2) => {
      val d1 = distance(t1, target)
      val d2 = distance(t2, target)
      d1 < d2
    }).take(k).map(_.answer)
    val answers = knears.toSet
    val verdict = answers.map(ans => ans -> knears.count(_ == ans))
    verdict.maxBy(tuple => tuple._2)._1
  }
}

object Use_kNN {
  def main(args: Array[String]) {
    println("Hellow World!!")
    val samples = Seq(
      Data(Seq(-0.10, +0.50), 1), Data(Seq(-0.45, +1.30), 1),
      Data(Seq(+0.60, +0.75), 1), Data(Seq(+0.30, +1.25), 1),
      Data(Seq(+0.75, +0.70), 1), Data(Seq(+1.20, +0.55), 1),
      Data(Seq(+0.20, +0.30), 0), Data(Seq(+1.60, +0.60), 0),
      Data(Seq(+0.40, +0.55), 0), Data(Seq(+0.60, +0.40), 0),
      Data(Seq(+0.80, +0.55), 0), Data(Seq(+1.25, +0.20), 0))

    val euclid = (a: Data, b: Data) => Math.sqrt(
      Math.pow(a.point(0) - b.point(0), 2) +
        Math.pow(a.point(1) - b.point(1), 2)
    )
    val knn = new KNN(4, samples, euclid) // k = 3
    println(knn.predict(Data(Seq(0.5, 0.3))))
    println(knn.predict(Data(Seq(0.2, 0.8))))


  }

}


