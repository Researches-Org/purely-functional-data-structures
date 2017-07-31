package util

object Math {

  def log2(d: Double): Double = math.log(d) / math.log(2)

  def main(args: Array[String]): Unit = {
    println(log2(5))
  }
}
