package stream

object tstStream {
  def main(args: Array[String]): Unit = {
    print(Stream(1,2,3).take(2).toList)
  }
}
