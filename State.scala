import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (i, r) = rng.nextInt
  (if (i < 0) -(i + 1) else i, r)
}


def double(rng: RNG): (Double, RNG) = {
  val (i, r) = nonNegativeInt(rng)
  (i / Int.MaxValue.toDouble + 1, r)
}

def intDouble(rng: RNG): ((Int, Double), RNG) = {
  val(i, r) = nonNegativeInt(rng)
  val (a, b) = double(r)
  ((i, a), b)
}

def doubleInt(rng: RNG): ((Double, Int), RNG) = {
  val(i, r) = double(rng)
  val(a, b) = nonNegativeInt(r)
  ((i, a), r)
}


def double3(rng: RNG): ((Double, Double, Double), RNG) = {
  val(i, r) = double(rng)
  val(a, b) = double(r)
  val(c, d) = double(b)
  ((i, a, c), d)
}

def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  @annotation.tailrec
  def go(max: Int, c: Int, l: List[Int], rng: RNG): (List[Int], RNG) = {
    c match {
      case `max` => (l, rng)
      case _ => {
        val (i, r) = nonNegativeInt(rng)
        go(`max`, c + 1, l ::: (i::Nil), r)
      }

    }
  }
  go(count, 0, Nil: List[Int], rng)
}

val rng = SimpleRNG(42)
ints(4)(rng)

type Rand[+A] = RNG => (A, RNG)

def unit[A](a: A): Rand[A] =
  rng => (a, rng)

val int: Rand[Int] = _.nextInt


def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
  rng => {
    val (a, rng2) = s(rng)
      (f(a), rng2)
  }


def doubleWithMap: Rand[Double] =
  map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))


def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
  rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f((a, b)), rng2)
  }

//We can used this combinator to express values more succinctly
def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
  map2(ra, rb)((_, _))
//We can reimplement intDouble and doubleint from execise 6.3 more succintly
val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

def sequence[A](as: List[Rand[A]]): Rand[List[A]] =
  as.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

def _ints(count: Int): Rand[List[Int]] =
  sequence(List.fill(count)(int))

def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
  rng => {
    val (a, b) = f(rng)
    g(a)(b)
  }

def nonNegativeLessThan(n: Int): Rand[Int] = {
  flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }
}

def mapViaFlat[A, B](s: Rand[A])(f: A => B): Rand[B] =
  flatMap(s)(i => unit(f(i)))

def map2ViaFlat[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
  flatMap(ra)(i => map(rb)(j => f(i, j)))
