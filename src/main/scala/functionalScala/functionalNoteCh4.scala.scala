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
}
case class Some[+A](get: A) extends Option[A] 
case object None extends Option[Nothing] 
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
}