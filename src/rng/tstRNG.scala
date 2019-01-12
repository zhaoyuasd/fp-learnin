package rng

object tstRNG extends  App{
  val rng=SimpleRNG(42)
  val (n1,rng1)=rng.nextInt
  println(n1)
  val (n2,rng2)=rng1.nextInt
  println(n2)
}
