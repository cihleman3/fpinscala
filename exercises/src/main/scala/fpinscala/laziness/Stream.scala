package fpinscala.laziness
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

import fpinscala.laziness.Stream.{empty, _}

trait Stream[+A] {
  def foldRight[B](
      z: => B
  )(
      f: (A, => B) => B
  ): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) =>
        f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def foldLeft[B](z: => B)(g: (=> B, A) => B): B =
    this match {
      case Cons(h, t) => g(t().foldLeft(z)(g), h())
      case _          => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) =>
      p(a) || b
    ) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def toList1: List[A] = {
    foldRight[List[A]](Nil) { (a, acc) => a :: acc }
  }

  def toList2: List[A] = {
    foldLeft[List[A]](Nil) { (acc, a) => a :: acc }
  }

  def toList: List[A] = {
    val buf = ListBuffer.empty[A]
    @tailrec
    def go(s: Stream[A]): List[A] =
      s match {
        case Cons(h, t) => buf += h(); go(t())
        case _          => buf.toList
      }

    go(this)
  }

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] =
    this match {
      case Empty      => None
      case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
    }

  def take(n: Int): Stream[A] = {
    def go(s: Stream[A], n: Int): Stream[A] =
      if (n <= 0) empty
      else
        s match {
          case Cons(h, t) => cons(h(), go(t(), n - 1))
          case empty      => empty
        }

    go(this, n)
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n >= 1 => t().drop(n - 1)
      case _                    => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _                    => empty
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, acc) => {
//      println("checking " + a)
      p(a) && acc
    })

  // implement with foldRight
  def takeWhile1(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty) { (a, todo) => if (p(a)) cons(a, todo) else empty }

  def headOption: Option[A] =
    foldRight[Option[A]](None) { (a, _) => Some(a) }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](empty) { (a, todo) => cons(f(a), todo) }

  def filter(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty) { (a, todo) => if (p(a)) cons(a, todo) else todo }

  def append[B >: A](bs: => Stream[B]): Stream[B] =
    foldRight[Stream[B]](bs) { (a, todo) => cons(a, todo) }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](empty) { (a, todo) => f(a) append todo }

  // 13.: Use  unfold  to  implement  map,  take,  takeWhile,  zipWith
  def mapViaUnfold[B](f: A => B): Stream[B] = {
    val foo: Stream[A] => Option[(B, Stream[A])] = {
      case Empty      => None
      case Cons(h, t) => Some(f(h()), t())
    }
    unfold(this)(foo)
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    type S = (Int, Stream[A])
    val f: S => Option[(A, S)] = {
      case (i, _) if i <= 0 => None
      case (_, Empty)       => None
      case (i, Cons(a, as)) => Some(a(), i - 1 -> as())
    }
    unfold(n -> this)(f)
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    type S = Stream[A]
    val f: S => Option[(A, S)] = {
      case Cons(a, as) if p(a()) => Some(a(), as())
      case _                     => None
    }
    unfold(this)(f)
  }

  def zipWith[B, C](bs: Stream[B])(combine: (A, B) => C): Stream[C] = {
    type S = (Stream[A], Stream[B])
    val f: S => Option[(C, S)] = {
      case (Cons(a, as), Cons(b, bs)) => Some((combine(a(), b()), (as(), bs())))
      case _                          => None
    }
    unfold(this, bs)(f)
  }

  // special case of `zipWith`
  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))


  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = {
    type S = (Stream[A], Stream[B])
    val f: S => Option[((Option[A], Option[B]), S)] = {
      case (Cons(a, as), Cons(b, bs)) => Some(Some(a()) -> Some(b()), as() -> bs())
      case (Empty, Cons(b, bs))       => Some((None, Some(b())), (Empty, bs()))
      case (Cons(a, as), Empty)       => Some((Some(a()), None), (as(), Empty))
      case _                          => None
    }
    unfold((this, bs))(f)
  }

  def startsWith[B](bs: Stream[B]): Boolean = {
    val ps = zipAll(bs)
    val p: ((Option[A], Option[B])) => Boolean = {
      case (Some(a), Some(b)) => a == b
      case (_, None)          => true
      case _                  => false
    }
    ps.forAll(p)
  }

  def tails: Stream[Stream[A]] = {
    type S = Stream[A]
    val f: S => Option[(Stream[A], S)] = {
      case s @ Cons(_, as) => Some(s, as())
      case _           => None
    }
    val e: Stream[Stream[A]] = empty
    val e2: Stream[Stream[A]] = Stream(empty)
    unfold(this)(f) append e2
  }

  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] =
    foldRight(Stream(z)){ (a, acc) => cons(f(a, acc.headOption.get), acc) }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](c: A): Stream[A] = Stream.cons(c, constant(c))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // 0, 1, 1, 2, 3, 5, 8,....
  val fib: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

  /** Corecursion. It takes an initial state,
   * and a function for producing both the next state and the next value in the generated stream.
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).fold(empty[A]) { case (a, s) => cons(a, unfold(s)(f)) }
  }

  val ones1: Stream[Int] = unfold(1)(s => Some((1, s)))

  def constant1[A](c: A): Stream[A] = unfold(c)(s => Some((c, s)))

  def from1(n: Int): Stream[Int] = unfold(n)(n => Some((n, n + 1)))

  val fib1: Stream[Int] = unfold[Int, (Int, Int)]((0, 1)) { case (s, t) => Some((s + t, (t, s + t))) }

  val fibTill20: Stream[Int] = unfold[Int, (Int, Int)]((0, 1)) {
    case (s, t) => if (s <= 20) Some((s + t, (t, s + t))) else None
  }

  def main(args: Array[String]): Unit = {
    val s = Stream("a", "a'", "b", "a''")
    def isA(s: String) = s.startsWith("a")
    def isB(s: String) = s.startsWith("b")
    val b = Stream(1, 2)
    def foo(a: String, b: Int) = s"$a-$b"

//    val res =  Stream(3).scanRight(0)(_ + _).toList
    val res =  Stream(1,2,3).scanRight(0)(_ + _).toList

    println(res)
    println("fini")
  }
}
