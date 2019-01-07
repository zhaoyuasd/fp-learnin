/**
  * Created by viruser on 2018/10/23.
  */
sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A],right:Tree[A]) extends Tree[A]
object TstTree {
 def size[A](tree:Tree[A]):Int=tree match {
     case Branch(l,r)=>size(l)+size(r)+1
     case Leaf(_)=>1
     case _ =>0
   }

  //求最大深度
 def maxDepth[A] (tree:Tree[A]):Int=tree match {
   case Branch(l,r)=>(1+maxDepth(l)).max(1+maxDepth(r))
   case Leaf(_)=>1
   case _ =>0
 }

  // 求最大值 尾递归
  def  maxium(tree:Tree[Int]):Int= 
    tree match {
      case Branch(l,r)=> maxium(l).max(maxium(r))
      case Leaf(value)=> value
    }


  // 值转换 尾递归
  def map[A,B](tree:Tree[A],f:A=>B):Tree[B]=
    tree match {
         case Leaf(a:A) =>Leaf(f(a))
         case Branch(left:Tree[A],right:Tree[A])=>Branch(map(left,f),map(right,f))
    }
}
