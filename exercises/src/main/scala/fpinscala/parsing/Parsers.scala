package fpinscala.parsing

import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.language.implicitConversions
import scala.util.matching.Regex

import fpinscala.testing.Prop.forAll
import fpinscala.testing.{Gen, Prop}

//import language.higherKinds

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  // always succeeds
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(a => succeed(f(a)))

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    p.flatMap(a => p2.map(b => f(a, b)))

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(Nil) else map2(p, listOfN(n - 1, p))(_ :: _)

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p.flatMap(a => p2.map(b => (a, b)))

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def slice: Parser[String] = self.slice(p)

    def **[B](p2: => Parser[B]): Parser[(A, B)] =
      self.product(p, p2)
    def product[B](p2: => Parser[B]): Parser[(A, B)] =
      self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] =
      self.flatMap(p)(f)
    def many = self.many(p)

  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
    def succeedLaw[A](a: A)(s: Gen[String]): Prop =
      forAll(s)(s => run(succeed(a))(s) == Right(a))
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.iterator.asScala.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location, String)] = List(), otherFailures: List[ParseError] = List()) {}

object Main extends App {
//    run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
  //  run("\d*\a*)("2aa") == Right("2aa")

//  val pds: Parser[String] = """\d*""".r
 /* pds
    .flatMap(ds =>
      ds.map(_.size)
        .map(i => listOfN(i, char('a')))
    )*/
}
