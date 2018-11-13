sealed  trait List[+A]
case object Nil extends  List[Nothing]
case class Cons[+A](head:A,tail:List[A]) extends  List[A]
object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, tail) => x + sum(tail)
  }


  def product(ds: List[Double]): Double = ds match {
    case Nil => 0.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  //删除首位
  def delFist[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  //替换头
  def setHead[A](ag: A, l: List[A]): List[A] = l match {
    case Nil => Cons(ag, Nil)
    case Cons(_, tail) => Cons(ag, tail)
  }

  //删除前几位
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else drop(delFist(l), n - 1)
  }


  //删除符合条件的
  def dropWhile[A](l: List[A], condition: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, tail) => {
      if (condition(h))
        dropWhile(tail, condition)
      else
        Cons(h, dropWhile(tail, condition))
    }
  }

  def dropWhile2[A](l: List[A])(condition: A => Boolean): List[A] = l match {
    case Cons(h, t) if condition(h) => dropWhile2(t)(condition)
    case _ => l
  }

  def tset(x: Int): Boolean = x >= 5

  // 将addition顺序添加到original前面
  def appendFront[A](original: List[A], addition: List[A]): List[A] = addition match {
    case Nil => original
    case Cons(h, tail) => Cons(h, appendFront(tail, original))
  }

  // 将addition顺序添加到original后面
  def appendBehind[A](original: List[A], addition: List[A]): List[A] = original match {
    case Nil => addition
    case Cons(h, tail) => Cons(h, appendBehind(tail, addition))
  }


  //右折叠 从最右端开始真正的计算
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(t, h) => f(t, foldRight(h, z)(f))
    case Cons(t, h) => foldLeft(h, f(t, z))((x, y) => f(y, x)) //使用foldLeft 来实现 foldRight
  }

  //使用右折叠计算链表长度
  def length[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(_, t) => foldRight(t, 1)((_, y) => y + 1) //参数对应 这里的下划线对应 t中的某一个元素  y对应1  这里的1是起始数据 后面会跟这进行变化

  }

  //左折叠 从最左端开始真正的计算
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
    case Cons(h, t) => foldRight(t, f(z, h))((x, y) => f(y, x)) //foldRight 来实现foldLeft
  }

  // 使用左折叠 求和 求积 求长度

  def sumLeft(as: List[Int]): Int = as match {
    case Nil => 0
    case Cons(h, t) => foldLeft(t, h)(_ + _) //左折叠 和右折叠在求和时 后面的f基本一致
  }

  def productLeft(as: List[Double]): Double = as match {
    case Nil => 1.0
    case Cons(h, t) => foldLeft(t, h)(_ * _)
  }

  def lengthLeft[A](as: List[A]): Int = as match {
    case Nil => 0
    case Cons(_, t) => foldLeft(t, 1)((x, _) => x + 1)
  }

  //使用fold来进行append

  def appendByFoldRight[A](ori: List[A], adi: List[A]): List[A] = ori match {
    case Nil => adi
    case Cons(_, _) => foldRight(ori, adi)(Cons(_, _)) //这个是遍历 ori将 ori顺序添加到adi前面
  }

  def appendByFoldleft[A](ori: List[A], adi: List[A]): List[A] = adi match {
    case Nil => ori
    case Cons(_, _) => foldRight(adi, ori)(Cons(_, _)) //这个是遍历adi 顺序添加到 ori前面
    //因为Cons 的构造器的原因导致 链表只能向前生长 不能向后添加
  }

  // int ==(int+1)
  def increaseByOne(as: List[Int]): List[Int] = {
    val result:List[Int]=Nil
    as match {
      case Nil => Nil
      case Cons(_, _)=>foldRight(as,result)((x,y)=>Cons(x+1,y))
    }
  }

  //数据类型泛化  例如 List[Int]=>List[String]
def map[A,B](as:List[A])(f:A=>B):List[B]= {
  val res:List[B]=Nil
  as match {
    case Nil => Nil: List[B]
    case Cons(_,_)=>foldRight(as,res)((x,y)=>Cons(f(x),y))
  }
}

  //数据过滤
  def  filter[A](as:List[A])(f:A=>Boolean):List[A]={
    val res:List[A]=Nil
    as match {
      case Nil=>res
      case Cons(t,_) if f(t) =>Cons(t,res)
    }
  }


  //扩展
  def filterMap[A,B](as:List[A])(f:A=>List[B]):List[B]={
    val res:List[B]=Nil
    as match {
      case Nil=> res
      case Cons(_,_)=>foldRight(as,res)((x,y)=>appendBehind(f(x),y))
      case Cons(_,_)=>foldLeft(as,res)((x,y)=>appendBehind(f(y),x))
    }
  }

  //使用filterMap实现filter  搞不出来
  /*def filterByMape[A](as:List[A])(f:A=>Boolean):List[A]={
    filterMap(as)(x=> {if(f(x))Cons(x,Nil)})
  }*/
 // List(1,2,3) List(4,5,6)=>List(5,7,9)
  def zip(as:List[Int],bs:List[Int]):List[Int]=
  {
     as match {
      case Nil=>Nil
      case Cons(h1,t1)=>{
        bs match {
          case Nil=>Nil
          case Cons(h2,t2)=>{
            Cons(h1+h2, zip(t1,t2))
          }
        }
      }
    }
  }

  //数据泛化
  def zipWith[A](as:List[A],bs:List[A])(f:(A,A)=>A):List[A]=
  {
    as match {
      case Nil=>Nil
      case Cons(h1,t1)=>{
        bs match {
          case Nil=>Nil
          case Cons(h2,t2)=>{
            Cons(f(h1,h2), zipWith(t1,t2)(f))
          }
        }
      }
    }
  }


  def main(args: Array[String]): Unit = {
    val x=List(1,2,5,3,4,5)
    val m=x match{
      case Cons(x,Cons(2,Cons(4,_)))=>x
      case Nil=>42
      case Cons(x,Cons(y,Cons(3,Cons(4,_))))=>x+y
      case Cons(h,t)=>h+sum(t)
      case _ =>101}

    //println(m)
    //println(delFist(x))
    //println(dropWhile(x,tset)) //这里面 作为两个独立参数 之间不存在关联关系 所以 无法推导参数类型
    //println(dropWhile2(x)(_>4))  //这里进行柯里化之后 可以进行参数类型的推导

    //println(foldRight(List(1,2,3,4),Nil:List[Int])(Cons(_,_))) //Cons(1,Cons(2,Cons(3,Cons(4,Nil)))) -- 从左向右 依次提取合并
    //println(foldLeft(List(1,2,3,4),Nil:List[Int])((x,y)=>Cons(y,x))) //Cons(1,Cons(2,Cons(3,Cons(4,Nil)))) -- 从左向右 依次提取合并
    //println(lengthLeft(x))
    //println(appendByFoldRight(List(1,2,3),List(4,5,6)))

    //println(increaseByOne(List(1,2,3)))
    //println(filterMap(List(1,2,3))(i=>List(i,i)))

    println(zip(List(1,2,3) ,List(4,5,6)))
  }
  }
