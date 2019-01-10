package stream

object tstStream {
  def main(args: Array[String]): Unit = {
    print(Stream(1,2,3).take(2).toList)
    print(Stream(1,2,3,2,4).takeWhileFoldRignt(_ >2).toList)
    print(Stream(1,2,3,2,4).takeWhile(_ >2).toList)
    print(Stream(0,1,2,3,2,4).headOptionfoldRight.toList)
    print(Stream(0,1).append(Stream(9,9,9)).toList)

  }
}
