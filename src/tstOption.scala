import scala.util.hashing.Hashing.Default

/**
  * Created by viruser on 2018/10/24.
  */
package com.laozhao {

 sealed trait Option[+A] {
    def map[B](f: A => B): Option[B]

    def flatMpa[B](f: A => Option[B]): Option[B]

    def getOrElse[B >: A](default: => B): B

    def orElse[B >: A](ob: => Option[B]): Option[B]

    def filter(f: A => Boolean): Option[A]
  }

 case class Some[+A](get:A) extends Option[A] {
   override def map[B](f: (A) => B): Option[B] = get match {
     //case None=> None
     case _ =>Some(f(get))
   }

   override def flatMpa[B](f: (A) => Option[B]): Option[B] = {
     get match {
     //  case None=> None
       case _=> f(get)
     }
   }

   override def getOrElse[B >: A](default: => B): B = {
     get match {
      // case None=> default
       case _=> get
     }
   }

   override def orElse[B >: A](ob: => Option[B]): Option[B] = {
     get match {
       //case None=> ob
       case _=> Some(get)
     }
   }

   override def filter(f: (A) => Boolean): Option[A] = {
     get match {
       //case None=> None
       case _  => {
         if (f(get) ) Some(get)
         else None
       }
     }
   }
 }
 case object None extends Option[Nothing] {
   override def map[B](f: (Nothing) => B): Option[B] = None

   override def flatMpa[B](f: (Nothing) => Option[B]): Option[B] = None

   override def getOrElse[B >: Nothing](default: => B): B = default

   override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = None

   override def filter(f: (Nothing) => Boolean): Option[Nothing] = None
 }

  object tstOption {
    case class Employee(name: String,departMent:String)
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

    def main(args: Array[String]): Unit = {
      println(looupByName("zhao").map(_.departMent))
    }
  }

}