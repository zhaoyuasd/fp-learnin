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
   case Branch(l,r)=>{
        l match {
          case Branch(_,_)=>1+maxDepth(l)
        }
       r match {
         case Branch(_,_)=>1+maxDepth(l)
       }
   }
   case Leaf(_)=>1
   case _ =>0
 }
}
