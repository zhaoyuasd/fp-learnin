package stream {

  import stream.Stream.cons

  sealed trait Stream[+A] {
      def apply[A](as: A*): Stream[A] = {
        if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
      }

      def empty[A]: Stream[A] = Empty

      def toList: List[A] = {
        def go(a: Stream[A], acc: List[A]): List[A] = a match {
          case Empty => acc
          case Cons(h, t) => go(t(), h() :: acc)
        }
           go(this, List()).reverse
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
  }

}

