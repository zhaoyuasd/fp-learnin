package stream {

  sealed trait Stream[+A] {

      def toList: List[A] = {
        def go(a: Stream[A], acc: List[A]): List[A] = a match {
          case Empty => acc
          case Cons(h, t) => go(t(), h() :: acc)
        }
           go(this, List()).reverse
      }

    // 说实话 到这虽然我写出来了 不过有点看不懂了  获取前n 个值
      def take[A](n:Int):Stream[A]=  {
       if(n==0) Empty
        this match {
          case Empty=>Empty
          case Cons(h,t)=>Cons(h,()=>t().take(n-1))
        }
     }

    // 删除前几个
      def drop[A](n:Int):Stream[A]= this match {
         case Empty =>Empty
         case Cons(_,t)=> if(n>0) t().drop(n-1) else this
      }

    // 返回符合条件的值
    def takeWhile[A](f:A=>Boolean):Stream[A]=this match {
      case Empty=>Empty
      case Cons(h,t)=> if(f(h())) Cons(h,()=>t().takeWhile(f)) else t().takeWhile(f)
    }



    }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = {
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
    }
  }

}

