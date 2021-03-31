package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    if (this == None) ob else this

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(v) if f(v) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs)
      .map { m => xs.map { x => math.pow(x - m, 2) }}
      .flatMap(mean)

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(x => b.map(y => f(x, y)))

  // Straight-forward: with patter-matching.
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case hd :: tl => sequence(tl).flatMap(as => hd.map(aa => aa +: as)) // Or use map2
  }

  // Bit more cunning: recall that folding abstracts over patter matching on a seq.
  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil)) { case (aa, acc) =>
      acc.flatMap(as => aa.map(_ :: as)) // Or use map2
    }

  // Realise that `sequence` is just a particular case of the more generic `traverse`.
  def sequence3[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil)) { case (aa, acc) =>
      acc.flatMap(bs => f(aa).map(_ :: bs)) // Or use map2
    }
}

object OptionTest {
  import Option._
  def main(args: Array[String]): Unit = {
    // Test sequence
    val l0 = Nil
    val l1 = List(None)
    val l2 = List(Some(1))
    val l3 = List(None, Some(1))
    val l4 = List(Some(1), None)
    val l5 = List(Some(1), Some(2))
    assert(sequence(l0) == Some(Nil))
    assert(sequence(l1) == None)
    assert(sequence(l2) == Some(List(1)))
    assert(sequence(l3) == None)
    assert(sequence(l4) == None)
    assert(sequence(l5) == Some(List(1, 2)))

    // Test traverse
    val l6 = Nil
    val l7 = List(0)
    val l8 = List(1)
    val l9 = List(0, 1)
    val l10 = List(1, 0)
    val l11 = List(1, 2)
    val fn: Int => Option[Int] = x => if(x == 0) None else Some(x * 2)
    assert(traverse(l6)(fn) == Some(Nil))
    assert(traverse(l7)(fn) == None)
    assert(traverse(l8)(fn) == Some(List(2)))
    assert(traverse(l9)(fn) == None)
    assert(traverse(l10)(fn) == None)
    assert(traverse(l11)(fn) == Some(List(2, 4)))
  }
}