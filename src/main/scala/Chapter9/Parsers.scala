package Chapter9

import Chapter8._

import scala.util.matching.Regex


trait Parser[T] {
}

trait Parsers[ParseError, Parser[+_]] { self =>

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def slice: Parser[String] = self.slice(p)
    def product[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def many1: Parser[List[A]] = self.many1(p)
  }

  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n-1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List.empty)

  def map[A,B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(x => succeed(f(x)))

  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    for {
      a <- p
      b <- p2
    } yield (a, b)

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])( f: (A,B) => C): Parser[C] =
    for {
      a <- p
      b <- p2
    } yield f(a, b)

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
    lazy val col = input.slice(0,offset+1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }
  }

  def errorLocation(e: ParseError): Location
  def errorMessage(e: ParseError): String

  object Laws {

    def forAnyChar(c: Char) =
      run(char(c))(c.toString) == Right(c)

    def forAnyString(s: String) =
      run(string(s))(s) == Right(s)

    def orLaw = {
      run(or(string("abra"),string("cadabra")))("abra") == Right("abra")
      run(or(string("abra"),string("cadabra")))("cadabra") == Right("cadabra")
    }

    def listOfNLaw = {
      run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
      run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
      run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")
    }

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Gen.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

//    def succeedLaw(a: =
//      run(succeed(a))(s) == Right(a)
  }
}
