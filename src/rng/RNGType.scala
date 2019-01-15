package rng

object RNGType {
  type Rand[+A]=RNG=>(A,RNG)
  def map[A,B](s:Rand[A])(f:A=>B):Rand[B]=
    rng=>{
      val (a,rng2)=s(rng)
      (f(a)->rng2)
    }
  /*def nonNegativeEven:Rand[Int]=
    map(nonNegativeInt)(i=>i-i%2)*/
}
