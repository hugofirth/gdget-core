/** gdget
  *
  * Copyright (c) 2016 Hugo Firth
  * Email: <me@hugofirth.com/>
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at:
  *
  * http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
  */
package org.gdget.data

import language.higherKinds
import cats.{Id, ~>}
import cats.free.Free
import cats.free.Free.liftF
import org.gdget.{Edge, Graph}

/** ADT representing a basic grammar for read-only queries over graphs (i.e. collection types which provide `org.gdget.Graph`
  * instances), along with a `cats.free.Free` Monad for the ADT.
  *
  * `QueryIO` is a free monad for `QueryOp` which must be "compiled" via a `cats.arrow.NaturalTransformation` instance,
  * turning `QueryOp` instances into instances of another monad (such as `cats.Id`).
  *
  * See [[http://typelevel.org/cats/tut/freemonad.html here]] for more information.
  *
  * @author hugofirth
  */
object query {

  sealed trait QueryOp[G[_, _[_]], V, E[_], A]

  object QueryOp {

    case class Get[G[_, _[_]], V, E[_]](vertex: V, g: G[V, E])(implicit val G: Graph[G], val E: Edge[E])
      extends QueryOp[G, V, E, Option[V]]

    case class GetWhere[G[_, _[_]], V, E[_]](cond: V => Boolean, g: G[V, E])(implicit val G: Graph[G], val E: Edge[E])
      extends QueryOp[G, V, E, List[V]]

    case class TraverseEdge[G[_, _[_]], V, E[_]](vertex: V, edge: E[V], g: G[V, E])
                                                (implicit gEv: Graph[G], eEv: Edge[E]) extends QueryOp[G, V, E, Option[E[V]]]

    case class TraverseInNeighbour[G[_, _[_]], V, E[_]](vertex: V, in: V, g: G[V, E])
                                                       (implicit gEv: Graph[G], eEv: Edge[E]) extends QueryOp[G, V, E, Option[V]]

    case class TraverseOutNeighbour[G[_, _[_]], V, E[_]](vertex: V, out: V, g: G[V, E])
                                                        (implicit gEv: Graph[G], eEv: Edge[E]) extends QueryOp[G, V, E, Option[V]]
  }

  import QueryOp._

  /** [[cats.free.Free]] Monad for Graph Queries based upon QueryOp ADT */
  type QueryIO[G[_, _[_]], V, E[_], A] = Free[QueryOp[G, V, E, ?], A]

  /** TODO: Copy how Doobie has a default interpreter which consumes a ResultSet to make a default interpreter which "consumes"
    * an object with an implicit Graph */
  def interpreter[G[_, _[_]], V, E[_]](implicit gEv: Graph[G], eEv: Edge[E]) = new (QueryOp[G, V, E, ?] ~> Id) {

    def apply[A](fa: QueryOp[G, V, E, A]): Id[A] =
      fa match {
        case Get(v, g) => {
          println(s"Get $v")
          Graph[G].getVertex(g, v)
        }
        case GetWhere(condition, g) => Graph[G].vertices(g).filter(condition).toList
        case TraverseEdge(v, e, g) => {
          println(s"Traverse edge $e of $v")
          val n = Graph[G].neighbourhood(g, v)
          n.fold(None: Option[E[V]])(_.edges.find(_ == e))
        }
        case TraverseInNeighbour(v, inV, g) => {
          println(s"Traverse incoming neighbour of $v")
          val n = Graph[G].neighbourhood(g, v)
          n.fold(None: Option[V])(_.in.keySet.find(_ == inV))
        }
        case TraverseOutNeighbour(v, outV, g) => {
          println(s"Traverse outgoing neighbour of $v")
          val n = Graph[G].neighbourhood(g, v)
          n.fold(None: Option[V])(_.out.keySet.find(_ == outV))
        }
      }
  }

  final case class GraphOp[G[_, _[_]], V, E[_]](graph: G[V, E])(implicit gEv: Graph[G], eEv: Edge[E]) {

    type QueryOpA[A] = QueryOp[G, V, E, A]

    def get(vertex: V): QueryIO[G, V, E, Option[V]] = liftF[QueryOpA, Option[V]](Get(vertex, graph))

    def getWhere(cond: V => Boolean): QueryIO[G, V, E, List[V]] = liftF[QueryOpA, List[V]](GetWhere(cond, graph))

    def traverseEdge(vertex: V, edge: E[V]): QueryIO[G, V, E, Option[E[V]]] =
      liftF[QueryOpA, Option[E[V]]](TraverseEdge(vertex, edge, graph))

    def traverseInNeighbour(vertex: V, in: V): QueryIO[G, V, E, Option[V]] =
      liftF[QueryOpA, Option[V]](TraverseInNeighbour(vertex, in, graph))

    def traverseOutNeighbour(vertex: V, out: V): QueryIO[G, V, E, Option[V]] =
      liftF[QueryOpA, Option[V]](TraverseOutNeighbour(vertex, out, graph))
  }

}
