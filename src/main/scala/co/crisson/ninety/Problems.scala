package co.crisson.ninety

import scala.annotation.tailrec

object ListProblems {

  def sub1(n: Int) = n - 1

  def add1(n: Int) = n + 1

  @tailrec
  def last[A](xs: List[A]): A = xs match {
    case a :: Nil  => a
    case a :: rest => last[A](rest)
  }

  @tailrec
  def penultimate[A](xs: List[A]): A = xs match {
    case a :: b :: Nil => a
    case a :: rest     => penultimate[A](rest)
  }

  @tailrec
  def nth[A](n: Int, xs: List[A]): A = xs match {
    case a :: rest if n == 0 => a
    case a :: rest           => nth(sub1(n), rest)
  }

  def length[A](xs: List[A]): Int = xs match {
    case Nil       => 0
    case a :: rest => 1 + length(rest)
  }

  def reverse[A](xs: List[A]): List[A] = xs match {
    case a :: Nil  => List(a)
    case a :: rest => reverse(rest) ++ (a :: Nil)
  }

  @tailrec
  def isPalindrome[A](xs: List[A]): Boolean = xs match {
    case a :: b :: c :: Nil => a == c
    case a :: b :: Nil      => a == b
    case a :: rest =>
      a == last(rest) && isPalindrome(rest.take(rest.length - 1))
  }

  def compress[A](xs: List[A]): List[A] = xs match {
    case a :: Nil                 => List(a)
    case a :: b :: rest if a == b => compress(b :: rest)
    case a :: b :: rest           => a :: compress(b :: rest)
  }

  /**
    * TODO
    */
  //  def pack(xs: List[Any]): List[List[Any]] = xs.foldLeft(List(List[Any]())) { (accum: List[List[Any]], x: Any) =>
  //    x match {
  //      case a if accum.head == Nil    => List(List(a))
  //      case a if accum.head.head == a => List(List(a))
  //      case a :: b :: rest if a != b  => List(a :: b :: Nil) ++ compress(List(b :: rest))
  //      case a :: b :: rest            => List(a :: compress(b :: rest))
  //    }
  //  }

  def duplicate[A](xs: List[A]): List[A] = xs match {
    case a :: Nil  => a :: a :: Nil
    case a :: rest => a :: a :: duplicate(rest)
  }

  def duplicateN[A](n: Int, xs: List[A]): List[A] = xs match {
    case a :: Nil if n == 0 => Nil
    case a :: Nil           => a :: duplicateN(sub1(n), a :: Nil)
    case a :: rest          => a :: duplicateN(sub1(n), a :: Nil) ++ duplicateN(n, rest)
  }

  def drop[A](n: Int, xss: List[A]): List[A] = {
    def doDrop(m: Int, xs: List[A]): List[A] = xs match {
      case Nil                       => Nil
      case a :: rest if add1(m) == n => doDrop(0, rest)
      case a :: rest                 => a :: doDrop(add1(m), rest)
    }

    doDrop(0, xss)
  }

  def split[A](x: Int, xs: List[A]): (List[A], List[A]) = {
    (xs.take(x), xs.takeRight(xs.length - x))
  }

  def slice[A](i: Int, k: Int, xs: List[A]): List[A] = xs match {
    case a :: rest if k == 0 => Nil
    case a :: rest if i > 0  => slice(i - 1, k - 1, rest)
    case a :: rest           => a :: slice(0, k - 1, rest)
    case _                   => Nil
  }

  def rotate[A](i: Int, xs: List[A]): List[A] = {
    val n = if (i > 0) i else xs.length + i

    if (i == 0) {
      xs
    } else {
      val (a, b) = split(n, xs)
      b ++ a
    }
  }

  /**
   * TODO
   */
  def removeAt[A](i: Int, xs: List[A]): (List[A], A) = ???

  def insertAt[A](x: A, i: Int, xs: List[A]): List[A] = xs match {
    case a :: rest if i == 0 => x :: a :: rest
    case a :: rest           => a :: insertAt(x, i - 1, rest)
    case _                   => Nil
  }
  
  def range(i: Int, k: Int): List[Int] = {
    if (i == k) k :: Nil
    else i :: range(i + 1, k)
  }
  
  /**
   * TODO
   */
  def randomSelect[A](i: Int, xs: List[A]): List[A] = ???
}


