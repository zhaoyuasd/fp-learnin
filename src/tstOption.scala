import scala.util.hashing.Hashing.Default

/**
  * Created by viruser on 2018/10/24.
  */
package com.laozhao {

 sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match
    {
      case Some(a:A)=> Some(f(a))
      case _ =>  None
    }

    def flatMpa[B](f: A => Option[B]): Option[B] =this match {
      case Some(a:A)=>f(a)
      case _ =>None
    }

    def getOrElse[B >: A](default: => B): B=
   this match {
     case Some(a:B)=>a
     case _ =>default
   }

    def orElse[B >: A](ob: => Option[B]): Option[B]= this match {
      case Some(a:B) =>Some(a)
      case _ =>ob
    }

   def add[A](a:Option[A]):Option[List[A]]=this match {
     case Some(a:List[A]) =>{
       a match {
         case None =>None
         case Some(b:A)=>Some(Nil::a::b)
       }
     }
     case None =>None
   }
    def filter(f: A => Boolean): Option[A] =this match {
      case Some(a:A)=> if(f(a))  Some(a) else None
      case _ =>None
    }

   def  sequence[A](a:List[Option[A]]):Option[List[A]]= a match {
     case Nil =>None

   }
  }
 case class Some[+A](get:A) extends Option[A]
 case object None extends Option[Nothing]

  object tstOption {
   def avg(xs:Seq[Double]):Option[Double]={
     case Nil=>None
     Some(xs.sum/xs.length)
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

}