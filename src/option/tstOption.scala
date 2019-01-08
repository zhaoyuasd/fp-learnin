
/**
  * Created by viruser on 2018/10/24.
  */
package option

sealed trait Option[+A] {
   //改变内部的值 函数只对内部的值进行改变生成的结果也只是的内部的值
    def map[B](f: A => B): Option[B] = this match
    {
      case Some(a:A)=> Some(f(a))
      case None =>  None
    }

   def map2[A,B,C](a:Option[A],b:Option[B])(f:(A,B)=>C):Option[C] =
    // a.map(aa=>b.map(bb=>f(aa,bb)))
     a.flatMpa(aa=>b.map(bb=>f(aa,bb)))


   // 使用内部的值生成新的option 原来的都丢弃 只保留内部状态的值同时配合函数生成新的option
    def flatMpa[B](f: A => Option[B]): Option[B] =this match {
      case Some(a:A)=>f(a)
      case None =>None
    }

    def getOrElse[B >: A](default: => B): B=
   this match {
     case Some(a:B)=>a
     case None =>default
   }

    def orElse[B >: A](ob: => Option[B]): Option[B]= this match {
      case Some(a:B) =>Some(a)
      case None =>ob
    }

   /*def add[A](a:Option[A]):Option[List[A]]=this match {
     case Some(a:List[A]) =>{
       a match {
         case None =>None
         case Some(b:A)=>Some(Nil::a::b)
       }
     }
     case None =>None
   }*/
    def filter(f: A => Boolean): Option[A] =this match {
      case Some(a:A)=> if(f(a))  Some(a) else None
      case None =>None
    }


   def  sequence[A](a:List[Option[A]]):Option[List[A]]= a match {
     case Nil =>Some(Nil)
     case head::tail => head.flatMpa(hh=>sequence(tail).map(hh::_))
   }

   def traverse[A,B](a:List[A])(f:A=>Option[B]):Option[List[B]]=
     a match {
       case Nil=>Some(Nil)
       case h::t =>f(h) .flatMpa(hh=>traverse(t)(f).map(hh::_))
     }
  }
 case class Some[+A](get:A) extends Option[A]
 case object None extends Option[Nothing]

  object tstOption {
   def avg(xs:Seq[Double]):Option[Double]={
     if(xs.isEmpty) None else  Some(xs.sum/xs.length)
   }

   def variance (xs:Seq[Double]):Option[Double]={
     avg(xs).flatMpa(m=>avg(xs.map(i=>math.pow(i-m,2))))
   }

    /*case class Employee(name: String,departMent:String)
    import scala.util.control.Breaks._

    val els:List[Employee]=List(Employee("zhao","hhh"),Employee("zhao2","hhh2"))
    def looupByName(name:String):Option[Employee]={
      var res :Option[Employee]=None
      for (i <- 0 to List.length(els)){
        breakable{
          if (els(i).name==name) {
            res = Some(els(i))
            break
          }
        }
      }
      res
    }
*/
    def main(args: Array[String]): Unit = {
     // println(looupByName("zhao").map(_.departMent))
    }
  }