package stream {
  sealed trait Stream[+A] {
      def toList: List[A] = {
        def go(a: Stream[A], acc: List[A]): List[A] = a match {
          case Empty => acc
          case Cons(h, t) => go(t(), h() :: acc)
        }
           go(this, List()).reverse
      }
      def take(n:Int):Stream[A]=this match {
        case Empty=>Empty
        case Cons(h, t)=>if(n>0) Cons(h,()=>t().take(n-1))  else Empty
      }

    def drop(n:Int):Stream[A]=this match {
      case Empty=>Empty
      case Cons(_, t)=> if(n>0) t().drop(n-1) else this
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
    def apply[A](as: A*): Stream[A] = {
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    }
     def empty[A]: Stream[A] = Empty

    def main(args: Array[String]): Unit = {
      print(Stream(1,2,3,4).take(2).toList)
      print(Stream(1,2,3,4).drop(2).toList)
    }

  }

}

