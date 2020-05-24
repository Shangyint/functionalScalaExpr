import Stream._

sealed trait Stream[+A] {
  def toListRec: List[A] = this match {
    case Empty => List[A]()
    case Cons(h, t) => h() :: t().toListRec
  }

  // this is still inefficient due to append to List
  def toList: List[A] = {
    var bufL = List[A]()
    @annotation.tailrec
    def go(l: Stream[A]): List[A] = l match {
      case Empty => bufL
      case Cons(h, t) => bufL = bufL :+ h(); go(t())
    }
    go(this)
  }

  def toListFast: List[A] = {
    // use ListBuffer, still need extra O(n)?
    ???
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  // if p(h()) evaluates to true, tail will not be traversed (short-circuit of ||)
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true 
    case Cons(h, t) if (p(h())) => t().forAll(p)
    case _ => false
  }

  def forAllViaFoldRight(p: A => Boolean): Boolean = 
    foldRight(true)((h, t) => if (p(h)) t else false)

  def forAllViaFoldRightAns(p: A => Boolean): Boolean = 
    foldRight(true)((h, t) => p(h) && t)

  def takeWhileViaFold(p: A => Boolean): Stream[A] = 
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def headOptionViaFold: Option[A] = foldRight(None: Option[A])((h, t) => Some(h))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))
  def filter(f: A => Boolean): Stream[A] = 
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[B>:A](as: => Stream[B]): Stream[B] = foldRight(as)(cons(_, _))
  // Wrong: def append(as: => Stream[A]): Stream[A] = foldRight(as)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight(empty[B])((h, t) => f(h).append(t))

  // ans: def append[B>:A](as: Stream[B]): Stream[B] = foldRight(as)(cons(_, _))

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

// use this object to track time of Stream
object BreakPoint {
  var printed = 0
  def break: Unit = {
    printed += 1
    println("BreakPoint"+printed)
  }
}


object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty
  // Recall that Scala uses subtyping to represent data constructors, 
  // but we almost always want to infer Stream as 
  // the type, not Cons or Empty. Making smart constructors that return the 
  // base type is a common trick.
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

object Test5 extends App {
  val lazyL = Stream(1, 2, 3, 4)
  val l = lazyL.toListRec

  println("LazyL toList " + lazyL.toList)
  println("\nEnd of Test 5")
}