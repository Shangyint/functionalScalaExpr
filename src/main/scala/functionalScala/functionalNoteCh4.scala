sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(get) => Some(f(get))
      case None => None
    }
    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Some(get) => f(get)
      case None => None
    }
    def getOrElse[B >: A](default: => B): B = this match {
      case Some(get) => get
      case None => default
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case Some(_) => this
      case None => ob
    }
    def filter(f: A => Boolean): Option[A] = this match {
      case Some(get) if f(get) => this 
      case _ => None
    }
    def isEmpty: Boolean = this match {
      case Some(get) => false
      case None => true
    }
}
case class Some[+A](get: A) extends Option[A] 
case object None extends Option[Nothing]

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(value) => Left(value)
    case Right(value) => Right(f(value))
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(value) => Left(value)
    case Right(value) => f(value)
  }
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(value) => b
    case Right(value) => Right(value)
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
    //for {e1 <- this; e2 <- b} yield f(e1, e2)
    this flatMap(aa => (b map (bb => f(aa, bb))))
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
// chapter 4
object Test4 extends App {
  def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = 
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
      case (Some(av), Some(bv)) => Some(f(av, bv))
      case _ => None
  }

  // ans:
  def map2Ans[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a flatMap (aa => b map (bb => f(aa, bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case head :: tl => f(head) flatMap(headB => traverse(tl)(f) map (headB :: _))
    // ans: h::t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  def traverseViaFold[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
    a.foldRight[Option[List[B]]](Some(Nil))((aa, bb) => map2(f(aa), bb)((opa, opb) => opa::opb))

  def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case head :: tl => for { h <- head; t <- sequence2(tl) } yield h :: t
  }

  def traverse3[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case h::t => (f(h) map2 traverse3(t)(f))(_ :: _)
  }

  def traverse2[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???

}