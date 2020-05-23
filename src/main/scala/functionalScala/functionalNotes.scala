package functionalScala

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

object Test1 extends App {
  println("Hello World")
}

object Test2 extends App {
  def factorial(n: Int): Int = {
    // functional loop, annotation to ensure tailrec
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n = n - 1, acc = n * acc)
    }

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, last: Int, res: Int): Int = {
      n match {
        case 0 => last
        case _ => go(n-1, res, last + res)
      }
    }

    go(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = n match {
        case _ if n == as.length - 1 => true
        case _ => 
          if (ordered(as(n), as(n+1))) loop(n + 1) else false
      }

    loop(0)
  }

  val < = (a: Int, b: Int) => a < b

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
  val plus1 = (a: Int) => a + 1
  val times2 = (a: Int) => a * 2

  println("factorial of 5 is " + factorial(5))
  println("fib of 5 is " + fib(9))
  println("This is a sorted array " + isSorted(Array(1,2,3,4,5), <))
  println("This is not a sorted array " + isSorted(Array(1,2,3,4,5,3), <))
  println("Compose plus1 and times2 (times2 first): comp(3) == " + compose(plus1, times2)(3))
  println("times2 andThen plus1: comp(3) == " + (times2 andThen plus1)(3))
}

object Test3 extends App {
  // question: page 30 List[+A], what does +A mean?

  def tail[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    // ans: case Nil => sys.error("tail of empty list")
    case Cons(x, xs) => xs
    // ans: case Cons(_, t) => t 
  }

  def setHead[A](ls: List[A], rep: A): List[A] = ls match {
    case Nil => sys.error("Set head of empty list")
    case Cons(_, xs) => Cons(rep, xs)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => l match {
      case Nil => sys.error("drop more items then the list has")
      // ans: case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    // Wrong: case Nil => Nil
    // Wrong: case Cons(head, tail) if f(head) => tail 
    case Cons(head, tail) if f(head) => dropWhile(tail, f) 
    case _ => l
  }

  val xs: List[Int] = List(1,2,3,4,5)
  val ex1 = dropWhile(xs, (x: Int) => x < 4)

  @annotation.tailrec
  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
    // Wrong: case Nil => Nil
    // Wrong: case Cons(head, tail) if f(head) => tail 
    case Cons(head, tail) if f(head) => dropWhile2(tail)(f) 
    case _ => l
  }
  // now anon function type can inferred
  val ex2 = dropWhile2(xs)(x => x < 4)

  // can't do @annotation.tailrec
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    // ans: case Nil => sys.error("init of empty list")
    case Cons(head, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail)) 
  }

  // the previous init is not a tailrec thus cannot be optimized 
  def init2[A](l: List[A]): List[A] = {
    var buffer = ListBuffer[A]()
    @annotation.tailrec
    def go(our: List[A]): List[A] = our match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => List(buffer.toList: _*)
      case Cons(head, tail) => {
        buffer += head
        go(tail)
      }
    }
    go(l)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(head, tail) => f(head, foldRight(tail, z)(f))
  }

  println("foldRight get original list " + 
    foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
  
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((a, b) => b + 1)
  }
  println("length of [1,2,3] is " + length(List(1,2,3)))

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  val sum2 = (l: List[Int]) => foldLeft(l, 0)(_ + _)
  println("sum of [1,2,3] is " + sum2(List(1,2,3)))
  val product2 = (l: List[Double]) => foldLeft(l, 1.0)(_ * _)
  println("product of [1,2,3,4] is " + product2(List(1,2,3,4)))

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((ls, h) => Cons(h, ls))
  }
  println("reverse of list [1,2,3] is " + reverse(List(1,2,3)))

  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(head, tail) => z
  }
}
