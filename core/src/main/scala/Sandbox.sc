import org.gdget.data._
import org.gdget.std.all._

val a = Map(
  1 -> (Set(2,3), Set(4,5,6)),
  2 -> (Set(5,3), Set(1)),
  3 -> (Set(6,4), Set(2,1)),
  4 -> (Set(1), Set(3)),
  5 -> (Set(1), Set(2,6)),
  6 -> (Set(1,5), Set(3))
)

type E[A] = (A, A)

val b = SimpleGraph[Int, E](
  1 -> 4,
  1 -> 5,
  1 -> 6,
  2 -> 1,
  3 -> 2,
  3 -> 1,
  4 -> 3,
  5 -> 2,
  5 -> 6,
  6 -> 3
)
//
//import cats._
//import cats.std.all._
//import cats.syntax.eq._
//
//implicit def tuple2Eq[A: Eq, B: Eq]: Eq[(A, B)] = new Eq[(A, B)] {
//  def eqv(x: (A, B), y: (A, B)) = x._1 === y._1 && x._2 === y._2
//}
//
//
//val a = (1, 2)
//val b = (2, 3)
//val c = ("Hello", 4)
//
//a === b
//
//1 === 2

