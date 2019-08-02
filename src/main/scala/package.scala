import scala.util.Try

package object draughts {

  sealed trait NonEmptyList[+A] {
    def head: A
  }
  case class NonEmptyCons[+A](head: A, tail: NonEmptyList[A]) extends NonEmptyList[A]
  case class NonEmptySingle[+A](head: A) extends NonEmptyList[A]

  sealed trait Validation[+E, +A] {
    def isFailure: Boolean = this.isInstanceOf[Failure[_]]
    def isSuccess: Boolean = this.isInstanceOf[Success[_]]
    def map[C, EE >: E](f: A => C): Validation[EE, C] = flatMap(a => Success(f(a)))
    def flatMap[C, EE >: E](f: A => Validation[EE, C]): Validation[EE, C] = fold(e => Failure[EE](e), a => f(a))
    def fold[C](fe: E => C, fa: A => C): C = this match {
      case Failure(e) => fe(e)
      case Success(a) => fa(a)
    }
    def toOption: Option[A] = fold(_ => None, Some(_))
  }
  case class Failure[+E](e: E) extends Validation[E, Nothing]
  case class Success[+A](e: A) extends Validation[Nothing, A]

  type Failures = NonEmptyList[String]
  type Valid[+A] = Validation[Failures, A]

  def success[A](e: A): Valid[A] = Success(e)
  def failure(s: String): Valid[Nothing] = Failure(NonEmptySingle(s))
  def failure(fs: Failures): Valid[Nothing] = Failure(fs)

  val White = Color.White
  val Black = Color.Black

  type Direction = (Int, Pos => Option[Pos])
  type Directions = List[Direction]

  type PieceMap = Map[Pos, Piece]

  type PositionHash = Array[Byte]

  object implicitFailures {
    implicit def stringToFailures(str: String): Failures = NonEmptySingle(str)
  }

  def parseIntOption(str: String): Option[Int] =
    Try(Integer.parseInt(str)).toOption

}
