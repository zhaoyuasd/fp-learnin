package list

/**
  * Created by viruser on 2018/10/22.
  */

object tstList {
  def sum(ints:list.List[Int]):Int =ints match {
    case list.Nil=>0
    case Cons(x,tail)=>x+sum(tail)
  }

  def main(args: Array[String]): Unit = {
    val x=list.List(1,2,5,3,4,5) match{
      case Cons(x,Cons(2,Cons(4,_)))=>x
      case list.Nil=>42
      case Cons(x,Cons(y,Cons(3,Cons(4,_))))=>x+y
      case Cons(h,t)=>h+sum(t)
      case _ =>101}
    print(x)
  }
}
