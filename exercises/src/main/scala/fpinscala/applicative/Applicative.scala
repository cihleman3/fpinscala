package fpinscala
package applicative

import scala.language.implicitConversions

import fpinscala.applicative.Applicative.Id
import fpinscala.applicative.Monad.idMonad
import fpinscala.applicative.StateUtil._
import fpinscala.applicative.Traverse.{listTraverse, treeTraverse}
import fpinscala.laziness.Stream
import fpinscala.monads.Functor
import fpinscala.monoids._
import fpinscala.state._

trait Applicative[F[_]] extends Functor[F] {

  def unit[A](a: => A): F[A]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val foo: F[A => B => C] = unit(f.curried)
    val g: F[B => C] = apply(foo)(fa)
    apply(g)(fb)
  }

  def map2_alt[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = {
    val foo: (A => B, A) => B = _(_)
    map2(fab, fa)(foo)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] = {
    val z: F[List[A]] = unit(Nil)
    fas.foldRight(z) { (a, acc) => map2(a, acc)(_ :: _) }
  }

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    sequence(as.map(f))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val foo: F[A => B => C => D] = unit(f.curried)
    val g: F[B => C => D] = apply(foo)(fa)
    val h = apply(g)(fb)
    apply(h)(fc)
  }

  def product[G[_]](G: Applicative[G]): Applicative[({ type f[x] = (F[x], G[x]) })#f] =
    new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
      def unit[A](a: => A): (F[A], G[A]) = {
        val fa: F[A] = Applicative.this.unit(a)
        val ga: G[A] = G.unit(a)
        (fa, ga)
      }
      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = {
        Applicative.this.map2(fa._1, fb._1)(f) -> G.map2(fa._2, fb._2)(f)
      }
    }

  def compose[G[_]](G: Applicative[G]): Applicative[({ type f[x] = F[G[x]] })#f] = {
    val self = this
    new Applicative[({ type f[x] = F[G[x]] })#f] {
      def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] = {
        val foo: (G[A], G[B]) => G[C] = (ga, gb) => G.map2(ga, gb)(f)
        self.map2(fa, fb)(foo)
      }

    }
  }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = {
    val z: F[Map[K, V]] = unit(Map())
    ofa.foldLeft(z) { case (acc, (k, fv)) =>
      map2(fv, acc) { (v, m) => m + (k -> v) }
    }
  }

}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(f))

  override def map[A, B](m: F[A])(f: A => B): F[B] =
    flatMap(m)(a => unit(f(a)))
  override def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))
}

object Monad {
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }
  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def eitherMonad[E] = new Monad[({ type f[x] = Either[E, x] })#f] {
    def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A, B](st: Either[E, A])(f: A => Either[E, B]): Either[E, B] = st match {
      case Right(a) => f(a)
      case Left(e)  => Left(e)
    }
  }

  def composeM[F[_], G[_]](implicit F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[({ type f[x] = F[G[x]] })#f] = {
    new Monad[({ type f[x] = F[G[x]] })#f] {
      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
      override def map[A, B](m: F[G[A]])(f: A => B): F[G[B]] = F.map(m)(G.map(_)(f))
      override def join[A](mma: F[G[F[G[A]]]]): F[G[A]] = {
        val boo:  G[F[G[A]]] =>  F[G[A]] = (gfg: G[F[G[A]]]) => F.map(T.sequence(gfg))(G.join)
        F.flatMap(mma)(boo)
      }
    }
  }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {

  type Id[A] = A

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.constant(a) // The infinite, constant stream

    override def map2[A, B, C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
        f: (A, B) => C
    ): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({ type f[x] = Validation[E, x] })#f] =
    new Applicative[({ type f[x] = Validation[E, x] })#f] {
      def unit[A](a: => A): Validation[E, A] = Success(a)
      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = {
        (fa, fb) match {
          case (Success(a), Success(b))         => Success(f(a, b))
          case Failure(e, es) -> Failure(f, fs) => Failure(f, (fs :+ e) ++ es)
          case (_, g @ Failure(_, _))           => g
          case (g @ Failure(_, _), _)           => g
        }

      }
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] // = sequence(map(fa)(f))
  def sequence[G[_]: Applicative, A](fma: F[G[A]]): G[F[A]] = {
    traverse(fma)(ma => ma)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    val f1: A => Id[B] = idMonad.unit(f(_))
    traverse(fa)(f1)(idMonad)
//    traverse[Id, A, B](fa)(f)(idMonad)
  }

  import Applicative._

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({ type f[x] = Const[B, x] })#f, A, Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) =>
      for {
        s1 <- get[S]
        (b, s2) = f(a, s1)
        _ <- set(s2)
      } yield b
    ).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, bcc) => () -> f(bcc, a))._2

  def fuse[G[_], H[_], A, B](
      fa: F[A]
  )(f: A => G[B], g: A => H[B])(implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
    traverse[({ type f[x] = (G[x], H[x]) })#f, A, B](fa)(a => (f(a), g(a)))(G product H)
  }

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({ type f[x] = F[G[x]] })#f] = {
    val self = this
    new Traverse[({ type f[x] = F[G[x]] })#f] {
      override def traverse[H[_], A, B](fa: F[G[A]])(f: A => H[B])(implicit H: Applicative[H]): H[F[G[B]]] = {
        self.traverse(fa)(ga => G.traverse(ga)(f))
      }
    }
  }

}

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](as: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      as.foldRight(G.unit(List[B]()))((a, fbs) => G.map2(f(a), fbs)(_ :: _))
//      sequence(map(as)(f))

    /*override def sequence[G[_], A](fma: List[G[A]])(implicit G: Applicative[G]): G[List[A]] = {
      println("seq")
      val z: G[List[A]] = G.unit(List.empty[A])
      fma.foldRight(z) { (ga, acc) => G.map2(ga, acc)(_ :: _) }
    }*/
  }

  val optionTraverse = new Traverse[Option] {
    /* override def sequence[G[_], A](fma: Option[G[A]])(implicit G: Applicative[G]): G[Option[A]] = {
      fma.fold(G.unit(Option.empty[A]))(ga => G.map(ga)(Some(_)))
    }*/
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      fa.fold(G.unit(Option.empty[B])) { a =>
        G.map(f(a))(Some(_))
      }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))

    /*override def sequence[G[_], A](tga: Tree[G[A]])(implicit G: Applicative[G]): G[Tree[A]] = {
      val u: List[G[Tree[A]]] = tga.tail.map(tga => sequence(tga))
      val foo: G[List[Tree[A]]] = listTraverse.sequence(u)
      G.map2(tga.head, foo)(Tree(_, _))
    }
     */
  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}

object ApplicativeMain extends App {
  val ls = List("a", "b", "c")
  val lz = listTraverse.zipWithIndex(ls)
  println(lz)

  def leaf[A](a: A): Tree[A] = Tree(a, List())
  val myTree = Tree(
    "a",
    List(
      Tree("b", List(leaf("e"), leaf("f"))),
      leaf("c"),
      Tree("d", List(leaf("g"), Tree("h", List(leaf("i"), leaf("j")))))
    )
  )
  val tz = treeTraverse.zipWithIndex(myTree)
  println(tz)
  println(treeTraverse.reverse(myTree))
  println(treeTraverse.foldLeft(myTree)("")(_ + _))
  val res = Tree(
    "j",
    List(
      Tree("i", List(Tree("h", List()), Tree("g", List()))),
      Tree("d", List()),
      Tree("c", List(Tree("f", List()), Tree("e", List(Tree("b", List()), Tree("a", List())))))
    )
  )

}
