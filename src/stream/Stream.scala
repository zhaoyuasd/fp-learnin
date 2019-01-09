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
      def take(n:Int):Stream[A]=  //这里如果方法为 take[A] 就会报错  take就运行
        this match {
          case Empty=>Empty
          case Cons(h,t)=>if(n>0) Cons(h,()=>t().take(n-1)) else Empty
        }


    // 删除前几个
      def drop(n:Int):Stream[A]= this match {  //这里如果方法为 drop[A] 就会报错  drop就运行
         case Empty =>Empty
         case Cons(_,t)=> if(n>0) t().drop(n-1) else this
      }

    // 返回符合条件的值
    def takeWhile(f:A=>Boolean):Stream[A]=this match { //这里如果方法为 takeWhile[A] 就会报错  takeWhile就运行
      case Empty=>Empty
      case Cons(h,t)=> if(f(h())) Cons(h,()=>t().takeWhile(f)) else t().takeWhile(f)
    }


    // 返回头部元素
    def headOption:Option[A]=this match{
      case Empty =>None
      case Cons(h,_)=>Some(h())
    }

    // 监测是否存在某元素 如果存在立即返回
    def exsist(f:A=>Boolean):Boolean= this match{
      case Empty=> false
      case Cons(h, t)=> f(h())||t().exsist(f)  //这里在验证true的时候 ||操作符是短路验证的 前面为true 后面就不进行验证了
    }


    // 右折叠  遍历元素 找到期望的结果后 立刻返回
    // =>B 表示f的第二个参数是传名参数 f不会立刻对其进行求值 直到实际使用之前
    def foldRight[B](z: =>B)(f:(A,=> B) =>B):B=this  match{
      case Empty =>z
      case Cons(h,t)=>f(h(),t().foldRight(z)(f))  //这里只要不对第二个参数进行求值 递归就不会发生
    }

    // 使用右折叠 实现exists方法  这个方法的后半段  foldRight(false)((a,b)=>f(a)||b) 有点看都不懂 无法理解
    def existfoldRight(f:A=>Boolean):Boolean=foldRight(false)((a,b)=>f(a)||b)  //这里面 这个b是什么类型 如何确定的  如何确定的

   //判断全部元素是否符合条件 如果有一个不符合立刻返回
    def forAll(f:A=>Boolean):Boolean=foldRight(true)((a,b)=>f(a)&&b)  // 如果f(a)为false 会立刻返回 不进行后续验证

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

