import Streamz.{cons, empty}

import Stream.cons
////////////////////
//    STREAMS     //
////////////////////

sealed trait Streamz[+A] {

  def toList: List[A] ={
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(as: Streamz[A]): List[A] = as match {
      case ::(x, y) =>
        buf += x()
        go(y())
      case _ => buf.toList
    }
    go(this)
  }

  def take(n: Int): Streamz[A] = this
  match {
    case ::(x, y) if n > 1  => cons(x(), y().take(n - 1))
    case ::(x, y) if n == 1 => cons(x(), empty)
    case _ => empty
  }

  def drop(n: Int): Streamz[A] = this
  match {
    case ::(x, y) if n > 1  => y().drop(n - 1)
    case ::(x, y) if n == 0 => this
    case ::(_ , y) => y()
  }

  def takeWhiles(p: A => Boolean): Streamz[A] = this
  match {
    case ::(x, y) if p(x()) => cons(x(), y().takeWhiles(p))
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case ::(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case ::(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def existsFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a,b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(false)((x, y) => p(x) && y)

  def takeWhileFR(p: A => Boolean): Streamz[A] =
    foldRight(empty[A])((x, y) => if (p(x)) cons(x, y) else empty )

  def headOption[A]: Option[A] =
    foldRight(None: Option[A])((x, y) => x match {
      case Empty => Option.empty
      case _ => Some(x)
    })

  def map[B](f: A => B): Streamz[B] = {
    foldRight(empty[B])((x, y) => cons(f(x), y))
  }

  def filter(p: A => Boolean): Streamz[A] =
    foldRight(empty[A])((x, y) => if (p(x)) cons(x, y) else y)

  def append[B >: A](a: Streamz[B]): Streamz[B] =
    foldRight(a)((x, y) => cons(x, y))

  def flatMap[B](f: A => Streamz[B]): Streamz[B] =
    foldRight(empty[B])((x, y) => cons(f(x)(), y))
}

case object Empty extends Streamz[Nothing]
case class ::[+A](h: () => A, f: () => Streamz[A]) extends Streamz[A]

object Streamz {

  def cons[A](hd: => A, tl: => Streamz[A]): Streamz[A] = {
    lazy val head = hd
    lazy val tail = tl
    ::(() => head, () => tail)
  }

  def empty[A]: Streamz[A] = Empty

  def apply[A](as: A*): Streamz[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail:_*))
}

Streamz(1, 2, 3, 4).take(3).toList

def constant[A](a: A): Streamz[A] = {
  lazy val inf: Streamz[A] = ::(() => a, () => inf)
  inf
}

def from(n: Int): Streamz[Int] = {
  cons(n, from(n + 1))
}

val fibs = {
  def go(f0: Int, f1: Int): Streamz[Int] =
    cons(f0, go(f1, f0+f1))
  go(0, 1)
}

def unfold[A, S](z: S)(f: S => Option[(A, S)]): Streamz[A] = {
  f(z) match {
    case Some((x, y)) => cons(x, unfold(y)(f))
    case None => empty
  }
}

val fibsViaUnfold =
  unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }

def fromViaFold(n: Int): Streamz[Int] = {
  unfold(n)(n => Some(n, n+1))
}

def constantFromFold[A](a: A): Streamz[A] = {
  unfold(a)(_ => Some(a, a))
}


def map[A, B](f: A => B): Streamz[B] = {
  unfold(this){
    case ::(h, t) => Some((f(h()), t()))
    case _ => None
  }
}
