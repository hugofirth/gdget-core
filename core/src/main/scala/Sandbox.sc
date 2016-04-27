import org.gdget.Graph
import org.gdget.data._
import org.gdget.std.all._
import language.higherKinds

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

import Query._

def query: Query[List[Vector[(Int, Int)]]] =
  for {
    v <- get(1)
    p <- withEdge(v, (1, 4))
  } yield p


import cats.{Id, ~>}
import SimpleGraph._

def toyInterpreter[G[_, _[_]], V, E[_]](g: G[V, E])(implicit ev: Graph[G]) = new (QueryA ~> Id) {
  def apply[A](fa: QueryA): Id[A] =
    fa match {
      case Get(v: V) => Graph[G].getVertex(g, v).fold(List.empty[V])(List(_)).asInstanceOf[A]
      case WithEdge(vs, e) => ???
      case WithInNeighbour(vs, inV) => ???
      case WithOutNeighbour(vs, outV) => ???
    }
}

