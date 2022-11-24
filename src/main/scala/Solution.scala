import scala.annotation.tailrec
import scala.util.Try

extension [A <: Any](a: A) def some: Option[A] = Some(a)

def last[A](xs: List[A]): A = xs.last

@tailrec
def lastRec[A](xs: List[A]): A =
  xs match
    case last :: Nil => last
    case _ :: tail   => lastRec(tail)
    case Nil         => throw new NoSuchElementException

// Find the last but one element of a list.
def penultimate[A](xs: List[A]): Option[A] =
  xs match
    case x :: _ :: Nil => x.some
    case _ :: tail     => penultimate(tail)
    case Nil           => None

// Find the Kth element of a list.
def nth[A](n: Int, xs: List[A]): Option[A] =
  @tailrec
  def loop[A](n: Int, xs: List[A]): Option[A] =
    xs match
      case x :: tail if n == 0 => x.some
      case _ :: tail           => loop(n - 1, tail)
      case Nil                 => None

  if n > xs.length || xs.isEmpty || n < 0 then None
  else loop(n, xs)

// Find the number of elements of a list.
def length[A](xs: List[A]): Int =
  def loop[A](acc: Int, xs: List[A]): Int =
    xs match
      case _ :: tail => loop(acc + 1, tail)
      case Nil       => acc

  loop(0, xs)

def reverse[A](xs: List[A]): List[A] =
  def loop(acc: List[A], xs: List[A]): List[A] =
    xs match
      case head :: tail => loop(head :: acc, tail)
      case Nil          => acc

  loop(Nil, xs)

def isPalindrome[A](xs: List[A]): Boolean =
  xs match
    case _ :: Nil | Nil                   => true
    case x :: tail if x.equals(tail.last) => isPalindrome(tail.init)
    case _                                => false

// Flatten a nested list structure.
def flatten(xs: List[Any]): List[Any] =
  @tailrec
  def loop(acc: List[Any], xs: List[Any]): List[Any] =
    xs match
      case head :: tail if head.isInstanceOf[List[_]] => loop(acc, head.asInstanceOf[List[Any]] ++ tail)
      case head :: tail                               => loop(acc :+ head, tail)
      case Nil                                        => acc

  loop(Nil, xs)

// Eliminate consecutive duplicates of list elements.
def compress[A](xs: List[A]): List[A] =
  @tailrec
  def loop[A](acc: List[A], xs: List[A]): List[A] =
    xs match
      case head :: tail => if acc.contains(head) then loop(acc, tail) else loop(acc :+ head, tail)
      case Nil          => acc

  loop(Nil, xs)

// Pack consecutive duplicates of list elements into sublists.
def pack[A](xs: List[A]): List[List[A]] =
  @tailrec
  def loop(acc: List[List[A]], xs: List[A]): List[List[A]] =
    xs match
      case head :: tail =>
        acc.lastOption match
          case Some(list) if list.contains(head) => loop(acc.init :+ (acc.last :+ head), tail)
          case _                                 => loop(acc :+ List(head), tail)
      case Nil => acc

  loop(Nil, xs)

// Pack consecutive duplicates of list elements into sublists without duplicate.
def packCompact[A](xs: List[A]): List[List[A]] = ???

// Run-length encoding of a list.
def encode[A](xs: List[A]): List[(Int, A)] =
  xs.foldLeft(List.empty[(Int, A)]) { (acc, elem) =>
    acc.lastOption match
      case Some((count, a)) if a.equals(elem) => acc.init :+ (count + 1, a)
      case _                                  => acc :+ (1 -> elem)
  }

// Modified run-length encoding.
def encodeModified[A](xs: List[A]): List[Any] =
  xs.foldLeft(List.empty) { (acc, elem) =>
    acc.lastOption match
      case Some(accElem) =>
        val pair = Try(accElem.asInstanceOf[(Int, A)]).toOption
        val a    = Try(accElem.asInstanceOf[A]).toOption
        (pair, a) match
          case (Some(count -> a), Some(_)) if a.equals(elem) => acc.init :+ (count + 1, a)
          case (None, Some(a)) if a.equals(elem)             => acc.init :+ (2 -> elem)
          case (_, _)                                        => acc :+ elem
      case _ => acc :+ elem
  }

// Decode a run-length encoded list.
def decode[A](xs: List[(Int, A)]): List[A] =
  xs.foldLeft(List.empty[A]) { (acc, elem) =>
    val (count, a) = elem
    acc ++ List.fill(count)(a)
  }

// Duplicate the elements of a list.
def duplicate[A](xs: List[A]): List[A] =
  xs.groupBy(identity).foldLeft(List.empty[A]) { (acc, pair) =>
    val (a, list) = pair
    acc ++ List.fill(list.size * 2)(a)
  }

// Duplicate the elements of a list a given number of times.
def duplicateN[A](n: Int, xs: List[A]): List[A] =
  xs.groupBy(identity).foldLeft(List.empty[A]) { (acc, pair) =>
    val (a, list) = pair
    acc ++ List.fill(list.size * n)(a)
  }
