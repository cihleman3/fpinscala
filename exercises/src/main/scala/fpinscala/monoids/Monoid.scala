package fpinscala.monoids

import scala.Function.const

import fpinscala.laziness.Stream
import fpinscala.monoids.Monoid._
import fpinscala.parallelism.Nonblocking._
import fpinscala.testing.Gen
import fpinscala.testing.Prop.run

trait Monoid[A] {
  def zero: A
  def op(a1: A, a2: A): A
}

object Monoid {

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1 compose a2
    override def zero: A => A = identity[A]
  }

  // data type from Part 2.
  //  trait Prop {}

  // data type from Part 2.

  import fpinscala.testing._
  import Prop._

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(gen)(a => m.op(a, m.zero) == a && m.op(m.zero, a) == a) &&
      forAll(Gen.listOfN(3, gen)) { case List(a, b, c) =>
        //        println(s"$a, $b, $c")
        m.op(a, m.op(b, c)) == m.op(m.op(a, b), c)
      }

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    ???

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val n = as.length
    if (n < 2) foldMap(as.toList, m)(f)
    else {
      val i = n / 2
      val (bs, cs) = as.splitAt(i)
      m.op(foldMapV(bs, m)(f), foldMapV(cs, m)(f))
    }
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val last = ints.last
    val int2 = ints.drop(1) :+ last
    val z: Seq[(Int, Int)] = ints zip int2
    val f: ((Int, Int), Boolean) => Boolean = {case ((i, j), b) => i <= j && b}
    foldRight(z.toList)(true)(f)
    foldMap(z.toList, endoMonoid[Boolean])(f.curried)(true)
  }
  //    foldMap(ints.toList, booleanAnd)(f)

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    ???

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    ???

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def zero: WC = Stub("")
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(c), Stub(d)) => Stub(c + d)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }
  }

  def count(s: String): Int = {
    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)
    def unstub(s: String) = s.length min 1
    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def zero: (A, B) = (A.zero, B.zero)
    override def op(p1: (A, B), p2: (A, B)): (A, B) = A.op(p1._1, p2._1) -> B.op(p1._2, p2._2)
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def zero: A => B = const(B.zero)
    override def op(f1: A => B, f2: A => B): A => B = a => B.op(f1(a), f2(a))
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    ???

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val emptyMap = Map[A, Int]()
    as.foldLeft(emptyMap) { (acc, a) => acc + (a -> (acc.getOrElse(a, 0) + 1)) }
  }
}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  def toList[A](as: F[A]): List[A] = foldRight(as)(List.empty[A])(_ :: _)

}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMap(as, mb)(f)
  override def toList[A](as: List[A]): List[A] = as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def toList[A](as: IndexedSeq[A]): List[A] = as.toList
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) = {
    val f1: (A, => B) => B = (a, b) => f(a, b)
    as.foldRight(z)(f1)
  }
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) = {
    def f1(b: => B, a: A): B = f(b, a)
    as.foldLeft(z)(f1)
  }
  override def toList[A](as: Stream[A]): List[A] = as.toList
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(a) => f(z, a)
    case Branch(left, right) =>
      val z1 = foldLeft(left)(z)(f)
      foldLeft(right)(z1)(f)
  }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
    case Leaf(a) => f(a, z)
    case Branch(left, right) =>
      val z1 = foldRight(right)(z)(f)
      foldRight(left)(z1)(f)
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(a) => mb.op(mb.zero, f(a))
  }
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
    case None => z
    case Some(a) => f(z, a)
  }
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    case None => z
    case Some(a) => f(a, z)
  }
}

object Main extends App {
  val g = Gen.choose(-1000, 1000)
  run(monoidLaws[Int](intAddition, g))
  val v = bag(Vector("a", "rose", "is", "a", "rose"))
  println(ordered(Seq(1, 2, 3, 4, 5, 6, 3).toIndexedSeq))
}
