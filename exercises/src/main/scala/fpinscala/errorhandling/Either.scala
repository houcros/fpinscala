package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Left(get) => Left(get)
   case Right(get) => Right(f(get))
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Left(get) => Left(get)
   case Right(get) => f(get)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Left(_) => b
   case Right(get) => Right(get)
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
   case (Left(g1), Left(g2)) => Left(g1)
   case (Left(g1), Right(g2)) => Left(g1)
   case (Right(g1), Left(g2)) => Left(g2)
   case (Right(g1), Right(g2)) => Right(f(g1, g2))
 }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](Right(Nil)) { case (e, acc) => f(e).map2(acc)(_ :: _) }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    es.foldRight[Either[E, List[A]]](Right(Nil)) { case (e, acc) => e.map2(acc)(_ :: _) }

  def sequence2[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}

object EitherTest {
  def main(args: Array[String]): Unit = {
    val e1: Either[String, Int] = Right(1)
    val e2: Either[String, Int] = Left("one")

    // map
    assert(e1.map(_ * 2) == Right(2))
    assert(e2.map(_ * 2) == Left("one"))
    // flatMap
    assert(e1.flatMap(_ => Right(1.0)) == Right(1.0))
    assert(e1.flatMap(_ => Left("uno")) == Left("uno"))
    assert(e2.flatMap(_ => Right(1.0)) == Left("one"))
    // orElse
    assert(e1.orElse(Right(1.0)) == Right(1))
    assert(e2.orElse(Right(1.0)) == Right(1.0))
    // map2
    val fn: (Int, Int) => Long = (x, y) => (x + y).toLong
    assert(e2.map2(e2)(fn) == Left("one"))
    assert(e1.map2(e2)(fn) == Left("one"))
    assert(e2.map2(e1)(fn) == Left("one"))
    assert(e1.map2(e1)(fn) == Right(2L))
  }
}