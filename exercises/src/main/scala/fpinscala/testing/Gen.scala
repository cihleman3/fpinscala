package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state.RNG.{Rand, mapF, nonNegativeLessThan}
import fpinscala.state._
import fpinscala.testing.Prop._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */

//trait Prop {}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop) = Prop { (m, n, rng) =>
    val r1: Result = run(m, n, rng)
    val r2 = p.run(m, n, rng)
    (r1, r2) match {
      case (f @ Falsified(_, _), _) => f
      case (_, f @ Falsified(_, _)) => f
      case _                        => Passed
    }
  }
  def ||(p: Prop) = Prop { (m, n, rng) =>
    run(m, n, rng) match {
      case Passed => Passed
      case _      => p.run(m, n, rng)
    }
  }
}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(as)(rng)
      .zip(Stream.from(0))
      .take(n)
      .map { case (a, i) =>
        try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) => f(n, rng) }

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(i => g(i))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    val casesPerSize = (n - 1) / max + 1
    val props: Stream[Prop] =
      Stream.from(0).take((n.min(max)) + 1).map(i => forAll(g(i))(f))
    val prop: Prop =
      props
        .map(p =>
          Prop { (max, n, rng) =>
            p.run(max, casesPerSize, rng)
          }
        )
        .toList
        .reduce(_ && _)
    prop.run(max, n, rng)
  }

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val range = stopExclusive - start + 1
    val foo: Rand[Int] = mapF(nonNegativeLessThan(range))(_ + start)
    Gen(State(foo))
  }
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = {
    val foo: Rand[Boolean] = mapF(nonNegativeLessThan(2))(_ > 0)
    Gen(State(foo))
  }
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))
  def listOf[A](f: Gen[A]): SGen[List[A]] = {
    SGen(i => Gen.listOfN(i, f))
  }
  def nonEmptyListOf[A](f: Gen[A]): SGen[List[A]] = {
    SGen(i => Gen.listOfN(i.max(1), f))
  }
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(first => if (first) g1 else g2)
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val die: Gen[Double] = Gen(State(s => RNG.double(s)))
    die.flatMap(x => if (x < g1._2) g1._1 else g2._1)
  }
}

/*
trait Gen[A] {
  def map[A, B](f: A => B): Gen[B] = ???
  def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
}
 */

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap[B](f(_).sample))
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(i => Gen.listOfN(i, this))
  /* A method alias for the function we wrote earlier. */
  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)
  def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    SGen(i => g(i).flatMap[B](f(_).g(i)))
  }
}

object Main extends App {
  val smallInt = Gen.choose(-10, 10)
  val maxProp: Prop = forAll(Gen.nonEmptyListOf(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }
  run(maxProp)

  val sortProp: Prop = forAll(Gen.listOf(smallInt)) { ls =>
    val ss = ls.sorted
    ss.length == ls.length && (0 until ls.length - 1).forall(i => ss(i) <= ss(i + 1))
  }
  run(sortProp)
}
