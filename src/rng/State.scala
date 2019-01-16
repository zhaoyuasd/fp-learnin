package rng

case class State[S,+A](run:S=>(A,S)){
  type  Rand[A] = State[RNG,A]
  def unit(a:A):State[Unit,A]=State[Unit,a]
}
