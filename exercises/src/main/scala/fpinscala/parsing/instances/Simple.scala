package fpinscala.parsing.instances

import scala.util.matching.Regex

import fpinscala.parsing.{Location, ParseError, Parsers}

object Simple {
  type Parser[+A] = Location => Result[A]

  sealed trait Result[+A]
  case class Success[+A](get: A, length: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]

  object SimpleParser extends Parsers[Parser] {
    override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???
    override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = ???
    override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???
    override def slice[A](p: Parser[A]): Parser[String] =
      (loc: Location) => {
        p(loc) match {
          case Success(get, length) => Success(get.toString, length)
          case f @ Failure(_)       => f
        }
      }

    override def succeed[A](a: A): Parser[A] =
      (_: Location) => Success(a, 0)

    def regex(r: Regex): Parser[String] =
      (loc: Location) => {
        val nextInput = loc.input.substring(loc.offset)
        if (r.matches(nextInput))
          Success(nextInput, nextInput.length + loc.offset)
        else
          Failure(loc.toError(s"$nextInput did not match $r"))
      }
    def string(s: String): Parser[String] =
      (loc: Location) => {
        val nextInput = loc.input.substring(loc.offset)
        if (nextInput.startsWith(s))
          Success(nextInput, nextInput.length + loc.offset)
        else
          Failure(loc.toError(s"$nextInput did not match $s"))
      }
  }
}
