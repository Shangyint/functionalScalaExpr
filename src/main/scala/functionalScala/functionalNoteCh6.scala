trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

case class State[S, +A](run: S => (A, S)) {
  import State._
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequenceViaFoldRight[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
      case Nil => (acc.reverse,s)
      case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }

  def apply[S, A](run:S => (A, S)):State[S, A] = State(run)

  def sequenceViaFoldLeft[S,A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List())) {
    (acc, f) => f.map2(acc)( _ :: _ )
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
  
  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

object Test6 extends App {
  type Rand[+A] = RNG => (A, RNG)
  type State[S,+A] = S => (A,S)
  // Wrong
  // def randomInt(n: Int): Int = {
  //   val (int, state) = SimpleRNG(n).nextInt
  //   int % (n + 1)
  // }

  // ans
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, rng2) = rng.nextInt
    (if (int < 0) -(int + 1) else int, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (int, rng2) = nonNegativeInt(rng)
    // Wrong: not including 1: (int.toDouble / Int.MaxValue, rng2)
    (int / (Int.MaxValue.toDouble + 1), rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int, rng2) = rng.nextInt
    val (dou, rng3) = double(rng2)
    ((int, dou), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case n if n <= 0 => (Nil, rng)
    case _ => {
      val (i, r) = rng.nextInt
      val (is, rFinal) = ints(count - 1)(r)
      (i :: is, rFinal)
    }
  }

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
    }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.foldLeft((Nil: List[A], rng))((lAndS, ra) => {
      val (a, rng1) = ra(lAndS._2)
      (lAndS._1 :+ a, rng1)
    })
  }

  // ans: very elegant!!!!
  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  val int: Rand[Int] = _.nextInt
  def _ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (i, r) = f(rng)
    g(i)(r)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt){ i =>
    val mod = i % n
    if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def mapState[S,A,B](a: State[S, A])(f: A => B): State[S, B] = s => {
    val (v, ns) = a(s)
    (f(v), ns)
  }

  def unitState[S,A](a: A): State[S, A] = s => (a, s)

  def map2State[S,A,B,C](a: State[S, A], b: State[S, B])
    (f: (A, B) => C): State[S, C] = {
      flatMapState(a)(va => mapState(b)(vb => f(va, vb)))
    }

  def flatMapState[S,A,B](f: State[S,A])(g: A => State[S,B]): State[S,B] = s => {
    val (v, ns) = f(s)
    g(v)(ns)
  }
  

  // TODO EXERCISE 6.11
}
