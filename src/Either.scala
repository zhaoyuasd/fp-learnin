


sealed trait Either[+E,+A] {
 def map[B](f:A=>B):Either[E,B] ={
   case Left(e:E)=>Left(e)
   case  Right(a:A)=>{
     try Right(f(a))
     catch{case ee:E=>Left(ee) }
   }
 }
}
case class Left[+E](value:E) extends Either[E,Nothing]
case class Right[+A](value:A) extends Either[Nothing,A]
