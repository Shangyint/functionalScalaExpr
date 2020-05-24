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
  // TODO Wrong: def append(as: => Stream[A]): Stream[A] = foldRight(as)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight(empty[B])((h, t) => f(h).append(t))

  // ans: def append[B>:A](as: Stream[B]): Stream[B] = foldRight(as)(cons(_, _))

  def mapViaFold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }
  
  def takeViaFold(n: Int): Stream[A] = 
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n-1)))
      case (Cons(h, _), n) if n == 1 => Some((h(), (empty, 0)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if (p(h())) => Some((h(), t()))
      case _ => None 
    }
  def zipWith[B, C](l2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, l2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // note: could use a -> b to create (a, b)
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (empty[A], t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), empty[B]))
      case (Empty, Empty) => None
    }

  def startsWith[A](s: Stream[A]): Boolean = {
    lazy val ts = unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((h1() == h2()) -> (t1(), t2()))
      case (Empty, Cons(_, _)) => Some(false -> (empty, empty))
      case _ => None
    }
    ts.foldRight(true)(_ && _)
  }

  def startsWithAns[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h,h2) => h == h2
    }

  def tails: Stream[Stream[A]] = 
    unfold(this) {
      case Cons(h, t) => Some(this -> t()) 
      case Empty => None
      // ans: added append
    } append(Stream(empty))

  // def scanRight(z: => B)(f: (A, => B) => B): Stream[B] =
  //   foldRight(z)()
  // ans:
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons.
      // So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
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

  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  // ans: more efficient

  def constant2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }
    
  def from(n: Int): Stream[Int] = cons(n, from(n+1))
  def fib(a: Int = 0, b: Int = 1): Stream[Int] = cons(a, fib(b, a + b))
  // ans TODO any difference?
  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0+f1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f)) 
    case None => empty
  }

  def fibsViaUnfold: Stream[Int] = 
    unfold((0, 1))(t => Some((t._1, (t._2, t._1 + t._2))))
    // ans: unfold((0, 1)) {case (a, b) => Some((a, (b, a + b))}
    // p => p match {case .. => } is equiv to {case .. => }

  def fromViaFold(n: Int): Stream[Int] = 
    unfold(n)(n => Some((n, n + 1)))

  def constantViaFold[A](a: A): Stream[A] = 
    unfold(a)(a => Some((a, a)))

  def onesViaFold: Stream[Int] = unfold(1)(n => Some((n, n)))

}

object Test5 extends App {
  println("Start of Test5")
  val lazyL = Stream(1, 2, 3, 4)
  println("LazyL toList " + lazyL.toList)
  val l = lazyL.toListRec
  val ones: Stream[Int] = Stream.cons(1, ones)
  val from1 = from(1)
  // TODO only h get evaluated?
  ones.takeWhile(_ == 1)


  val fibs10 = fib().take(10).toList
  println("Generated first 10 fibs: "+ fibs10)
  val fibsViaUnfold10 = fibsViaUnfold.take(10).toList
  println("Generated first 10 fibs via fold: "+ fibsViaUnfold10)

  println("ones starts with (1, 1, 1): "+ones.startsWith(Stream(1,1,1)))
  println("ones starts with (1, 1, 1): "+ones.startsWithAns(Stream(1,1,1)))
  println("(1,1,1) starts with ones: "+Stream(1,1,1).startsWith(ones))
  println("(1,1,1) starts with ones: "+Stream(1,1,1).startsWithAns(ones))
  println("\nEnd of Test 5")
}