package fpinscala.laziness

import fpinscala.laziness.Stream._
sealed trait Stream[+A] {
  override def equals(that: Any): Boolean = that match {
    case thatStream: Stream[A] => this.toList == thatStream.toList
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] =
    foldRight[List[A]](Nil)(_ :: _)

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => empty
  }

  def takeF(n: Int): Stream[A] =
    foldRight(empty[A]) { case (a, b) =>
      if (n > 0) cons(a, b.take(n - 1)) else empty
    }

  def takeU(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, _), 1) => Some(h() -> (empty, 0))
      case (Cons(h, t), s) if s > 1 => Some(h() -> (t(), s - 1))
      case _ => None
    }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def dropF(n: Int): Stream[A] =
    foldRight(empty[A]) { case (a, b) =>
      if (n > 0) b.drop(n - 1) else cons(a, b)
    }

  def takeWhile(p: A => Boolean): Stream[A] = {
    foldRight(empty[A]) { case (a, acc) =>
      if(p(a)) cons(a, acc.takeWhile(p)) else empty
    }
  }

  def takeWhileU(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h() -> t())
      case Empty => None
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true) { case (a, acc) => p(a) && acc }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def headOptionF: Option[A] =
    foldRight[Option[A]](None) { case (a, _) => Some(a) }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def mapU[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](b: Stream[B]): Stream[B] =
    foldRight(b)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold(this -> that) {
      case (_, Empty) => None
      case (Empty, _) => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()) -> (t1(), t2()))
    }

  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(this -> that) {
      case (Empty, Empty) => None
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None) -> (t1(), empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())) -> (empty, t2()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()) -> Some(h2())) -> (t1(), t2()))
    }

  def startsWith[B >: A](that: Stream[B]): Boolean =
    zipAll(that).takeWhile(_._2.isDefined).forAll { case (h1, h2) => h1 == h2 }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s@Cons(_, t) => Some(s -> t())
    } append Stream(empty)

  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] =
    foldRight(z -> Stream(z)) { case (h, (zz, s)) =>
      val acc = f(h, zz)
      val ss = cons(acc, s)
      acc -> ss
    }._2
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // This is more efficient than `cons(a, constant(a))` since it's just
  // one object referencing itself.
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(n: Int, m: Int): Stream[Int] =
      cons(n, go(m, n + m))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  def fromU(n: Int): Stream[Int] =
    unfold(n)(s => Some(s -> (s + 1)))

  def onesU: Stream[Int] =
    unfold(1)(_ => Some(1 -> 1))

  def constantU[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a -> a))

  def fibsU: Stream[Int] =
    unfold((0, 1)) { case (n, m) => Some(n -> (m, n + m)) }
}

object StreamTest {
  val s0 = empty[Int]
  val s1 = cons(1, empty)
  val s2 = cons(1, cons(2, empty))

  def main(args: Array[String]): Unit = {
    assert(s0.toList == Nil)
    assert(s2.toList == List(1, 2))

    assert(s0.take(1) == s0)
    assert(s2.take(0) == s0)
    assert(s2.take(1) == s1)
    assert(s2.take(2) == s2)

    assert(s0.drop(1) == s0)
    assert(s2.drop(0) == s2)
    assert(s2.drop(1) == cons(2, empty))
    assert(s2.drop(2) == s0)

    assert(s0.dropF(1) == s0)
    assert(s2.dropF(0) == s2)
   // assert(s2.dropF(1) == cons(2, empty))
    assert(s2.dropF(2) == s0)

    assert(s0.takeWhile(_ < 1) == s0)
    assert(s2.takeWhile(_ < 1) == s0)
    assert(s2.takeWhile(_ < 2) == s1)
    assert(s2.takeWhile(_ < 3) == s2)

    assert(s0.forAll(_ < 1))
    assert(s2.forAll(_ < 3))
    assert(!s2.forAll(_ < 2))
    assert(!s2.forAll(_ < 1))

    assert(s0.headOption.isEmpty)
    assert(s0.headOptionF.isEmpty)
    assert(s2.headOption.contains(1))
    assert(s2.headOptionF.contains(1))
  }
}