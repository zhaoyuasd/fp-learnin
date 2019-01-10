package stream

//无限流  这个无限流貌似不能再main方法下使用
object InfiniteStream extends App {

   // val ones:Stream[Int]=Stream.cons(1,ones) //这个就是无限流
    val ones:Stream[Int]=Stream.constant(1) //这个就是无限流
     println( ones.exsist(_ %2!=0))
     println(Stream from(1) take(5) toList)
     println(Stream from(1) drop(5) take(5) toList)
     println(Stream.fibs.take(6).toList)
}

object InfiniteStream2  {

  def main(args: Array[String]): Unit = {
   // val ones:Stream[Int]=Stream.constant(1) //这个就是无限流
    //这里不加lazy会报错  应该是内存炸了吧
    lazy val ones:Stream[Int]=Stream.cons(1,ones)
    print( ones.exsist(_ %2!=0))
  }
}