// monoid
// monoid laws
// - some type A
// - an associative binary operation, op
// - a value, zero s.t. op(x, zero) = x forall x: A

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2    
    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2    
    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2    
    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2   
    def zero: Boolean = true
  }
  
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 compose a2
    def zero: A => A = a => a    
  }

  // note
  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  // TODO EXERCISE 10.04

  // note
  // folding a monoid is the same in either direction
  
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = 
    as.foldRight(m.zero)((a, b) => m.op(f(a), b))
    // ans as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    // a => (b => f(a, b) can be simplified to f.curried
    foldMap(as, endoMonoid[B])(a => (b => f(a, b)))(z)

  // ans: the order matters
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)
  
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v.length match {
    case 0 => m.zero
    case 1 => f(v.head)
    case l@_ => m.op(foldMapV(v.take(l/2), m)(f), foldMapV(v.drop(l/2), m)(f))
  }

  // TODO Use foldMap to detect whether a given IndexedSeq[Int] is ordered. You’ll need
  // to come up with a creative Monoid.
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    // Our monoid tracks the minimum and maximum element seen so far
    // as well as whether the elements are so far ordered.
    val mon = new Monoid[Option[(Int, Int, Boolean)]] {
    def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) =
      (o1, o2) match {
      // The ranges should not overlap if the sequence is ordered.
        case (Some((x1, y1, p)), Some((x2, y2, q))) =>
          Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
        case (x, None) => x
        case (None, x) => x
      }
      val zero = None
    }
    // The empty sequence is ordered, and each element by itself is ordered.
    foldMapV(ints, mon)(i => Some((i, i, true))).map(_._3).getOrElse(true)
  }
  // foldMap()

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    val zero = Stub("")
    def op(a: WC, b: WC) = (a, b) match {
      case (Stub(c), Stub(d)) => Stub(c + d)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }
  }
  def count(s: String): Int = {
    def toWC(c: Char): WC = c match {
      case c if c.isWhitespace => Part("", 0, "")
      case _ => Stub(c.toString)
    }
    def strToInt(s: String): Int = if (s.isEmpty()) 0 else 1
    foldMapV(s.toIndexedSeq, wcMonoid)(toWC) match {
      case Stub(chars) => strToInt(chars)
      case Part(lStub, words, rStub) => 
        strToInt(lStub) + words + strToInt(rStub)
    }
  }
}

trait Foldable[F[_]] {
  import Monoid.{endoMonoid, dual}

  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B = 
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B = 
    foldMap(as)(a => b => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] =
    foldLeft(fa)(Nil: List[A])((la, a) => a :: la).reverse

  def toList2[A](fa: F[A]): List[A] =
    foldRight(fa)(Nil: List[A])(_ :: _)
}

// TODO Implement Foldable[List], Foldable[IndexedSeq], and Foldable[Stream].

// TODO Implement Foldable[Tree]

object Test10 extends App {

}