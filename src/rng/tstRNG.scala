package rng

object tstRNG extends  App{
  val rng=SimpleRNG(42)
  val rand= rng.nonNegativeEven
 val (n3,a3)=   rand.apply(rng)
  println(n3)
  val (n1,rng1)=rng.nextInt
  println(n1)
  val (n2,rng2)=rng1.nextInt
  println(n2)
}
