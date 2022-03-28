package fpinscala.state

import scala.annotation.tailrec

import fpinscala.state.RNG._
import fpinscala.state.State.{Rand => _, _}

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed =
        (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n =
        (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = { rng =>
    val t = rng.nextInt
    t
  }

  def unit1[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val min = Int.MinValue
    val (i, r) = rng.nextInt
    val j = if (i < 0) i + min else i
    (j, r)
  }

  // generates a Double between 0 and 1, not including 1
  def double(rng: RNG): (Double, RNG) = {
    val l = Int.MaxValue + 1.0
    val (i, r) = nonNegativeInt(rng)
    val d = i.toDouble / l
    (d, r)
  }

  def double2: Rand[Double] = {
    val l: Double = Int.MaxValue + 1.0
    val foo: Int => Double = _.toDouble / l
    map(nonNegativeInt)(foo)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    var ls: List[Int] = Nil
    var r = rng
    for (_ <- 1.to(count)) {
      val (i, r1) = r.nextInt
      ls = i :: ls
      r = r1
    }
    (ls.reverse, r)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    f(a, b) -> r2
  }

  def sequence1[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    val res: (Seq[A], RNG) = fs.foldLeft(Seq.empty[A] -> rng)((acc, ra) => {
      val (as, r) = acc
      val (a2, r2) = ra(r)
      (as :+ a2, r2)
    })
    res._1.toList -> res._2
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, s1) = f(rng)
    g(a)(s1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit1(mod) else nonNegativeLessThan(n)
    }

  def mapF[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit1(f(a)))
  }

  def map2F[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra) { a => mapF(rb)(f(a, _)) }
  }

}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = {
    flatMap(a => unit(f(a)))
  }
  def compose[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => sb.map { b => f(a, b) })
  }

  /** Alias for compose
    */
  def andThen[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = compose(sb)(f)

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })


}

object State {
  type Rand[A] = State[RNG, A]
  //  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))
  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    sequenceSeq(fs).map(_.toList)
  }
  def sequenceSeq[S, A](fs: Seq[State[S, A]]): State[S, Seq[A]] = {
    val zero: State[S, Seq[A]] = unit(Seq())
    fs.reverse.foldLeft(zero) { (acc, f) => f.andThen(acc)(_ +: _) }
  }

  // zum Testvergleich
  def sequenceRec[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    @tailrec
    def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
      actions match {
        case Nil    => (acc.reverse, s)
        case h :: t => h.run(s) match { case (a, s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s, sas, List()))
  }

  def sequenceViaFoldLeft[S, A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.compose(acc)(_ :: _))

  // The idiomatic solution is expressed via foldRight
  def sequenceViaFoldRight[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.compose(acc)(_ :: _))

}

object Main extends App {
  val r1: RNG = Simple(42)
  //  val r2: RNG = Simple(112)
  val int2: Rand[Int] = nonNegativeInt
  r1.nextInt
  /*for (_ <- range) {
    val (j, r) = double(r1)
    println(j)
    r1 = r
  }*/
//  println(map2(int, int2)((_, _))(r1))
//  println(map2F(int, int2)((_, _))(r1))
//  println(sequence1(List(int, int2))(r1))

  val simple1: State[RNG, Int] = State(int)
  val simple2: State[RNG, Int] = State(int2)
  val ls = List(simple1, simple2)
  val sorgenkind: State[RNG, Seq[Int]] = sequenceSeq(ls)
  val srf: State[RNG, List[Int]] = sequenceViaFoldRight(ls)
  val sr: State[RNG, List[Int]] = sequenceRec(ls)
  val (x, y): (Int, RNG) = simple2.run(r1)
  val (i1, r2): (Int, RNG) = simple1.run(r1)
  val (i2, r3): (Int, RNG) = simple2.run(r2)
//  println("---> " + (x, y))
  println((i1, r2))
  println((i2, r3))
  println("map2: " + simple1.compose(simple2)((_, _)).run(r1))
  println(sr.run(r1))
  println(srf.run(r1))
  println(sorgenkind.run(r1))
}

sealed trait Input
case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)
