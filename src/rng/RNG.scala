package rng


trait RNG {
 def nextInt:(Int,RNG)  //返回值是一个tuple2
}

case class SimpleRNG(seed:Long)extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n -> nextRNG) // 这个 -> 符号 表示组成二元组 这里写成（n,nextRNG)是一样的效果
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    ((if (i < 0) -(i + 1) else i) -> r)
    // 同样 这里完全 可以写成 这样  (if(i<0) -(i+1) else i , r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = rng.nextInt
    (i.toDouble -> r)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    // 这里方法名取list，会报错
    def mlist(count: Int, rng: RNG, list: List[Int]): (List[Int], RNG) = {
      if (count <= 0) (list, rng)
      else {
        val (i, r) = rng.nextInt
        mlist(count - 1, r, i :: list)
      }
    }

    mlist(count, rng, List(): List[Int])
  }

  type Rand[+A] = RNG => (A, RNG) //组合子  rand是一个过程的别名 本身是一种 函数类型
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = (rng: RNG) => {
    val (a, rng2) = s(rng)
    (f(a) -> rng2)
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  //?
  def map2[A, B, C](a: Rand[A], b: Rand[B])(f: (A, B) => C): Rand[C] = (rng) => {
    val (av, ar) = a(rng)
    val (bv, br) = b(ar)
    (f(av, bv), br) //这里有两个RNG 看不懂

    def unit[A](a: A): Rand[A] = rng => (a, rng)

// 这个想到用map2了  没想到foldright
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      fs.foldRight(unit(List[A]()))((f, ass) => map2(f, ass)(_ :: _))
  }
}

