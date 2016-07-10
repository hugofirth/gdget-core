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

import cats.data.Kleisli

import language.higherKinds
import cats.{Monad, ~>}
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

  /** Sum type / ADT for basic operations in a graph query.
    *
    * '''Note''': Early forms followed the [[http://typelevel.org/cats/tut/freemonad.html typelevel FreeMonad tutorial]] more
    * closely, however we want our interpreters to: a) be generic in terms of the Monad interpreted to
    * (`Id`, `s.c.Future` etc...); & b) consume a object with a `Graph` instance over which to actually execute a query.
    *
    * As a result we have moved to having our operations declare a method `defaultTransK` which takes type parameters
    * `M[_], G[_, _[_]], V, E[_]` (w. implicits `Monad[M]`, `Graph[G]`, `Edge[E]`). `defaultTransK` returns a
    * `Kleisli[M, G[V, E], A]`. Fair warning, this approach is lifted from how tpolecat does all things ''Free'' in
    * [[https://github.com/tpolecat/doobie Doobie]]. I don't yet understand all its nuance and potential pitfalls.
    *
    */
  sealed trait QueryOp[G[_, _[_]], V, E[_], A] {

    //TODO: Check .pure() is the right method here and equiv to Capture[M].apply in doobie
    def op[M[_]: Monad](f: G[V, E] => A)(implicit gEv: Graph[G], eEv: Edge[E]): Kleisli[M, G[V, E], A] =
      Kleisli((g: G[V, E]) => Monad[M].pure(f(g)))

    def defaultTransK[M[_]: Monad](implicit gEv: Graph[G], eEv: Edge[E]): Kleisli[M, G[V, E], A]
  }

  object QueryOp {

    case class Get[G[_, _[_]], V, E[_]](vertex: V) extends QueryOp[G, V, E, Option[V]] {
      override def defaultTransK[M[_] : Monad](implicit gEv: Graph[G], eEv: Edge[E]) = op(g => Graph[G].getVertex(g, vertex))
    }

    //TODO: Move to iterator here. Eventually replace Iterator with something better
    case class GetWhere[G[_, _[_]], V, E[_]](cond: V => Boolean) extends QueryOp[G, V, E, List[V]] {
      override def defaultTransK[M[_] : Monad](implicit gEv: Graph[G], eEv: Edge[E]): Kleisli[M, G[V, E], List[V]] =
        op(g => Graph[G].findVertices(g)(cond).toList)
    }

    //TODO: Switch to ===
    case class TraverseEdge[G[_, _[_]], V, E[_]](vertex: V, edge: E[V]) extends QueryOp[G, V, E, Option[E[V]]] {
      override def defaultTransK[M[_] : Monad](implicit gEv: Graph[G], eEv: Edge[E]): Kleisli[M, G[V, E], Option[E[V]]] =
        op { g =>
          val n = Graph[G].neighbourhood(g, vertex)
          n.fold(None: Option[E[V]])(_.edges.find(_ == edge))
        }
    }

    case class TraverseInNeighbour[G[_, _[_]], V, E[_]](vertex: V, in: V) extends QueryOp[G, V, E, Option[V]] {
      override def defaultTransK[M[_] : Monad](implicit gEv: Graph[G], eEv: Edge[E]): Kleisli[M, G[V, E], Option[V]] =
        op { g =>
          val n = Graph[G].neighbourhood(g, vertex)
          n.fold(None: Option[V])(_.in.keySet.find(_ == in))
        }
    }

    case class TraverseOutNeighbour[G[_, _[_]], V, E[_]](vertex: V, out: V) extends QueryOp[G, V, E, Option[V]] {
      override def defaultTransK[M[_] : Monad](implicit gEv: Graph[G], eEv: Edge[E]): Kleisli[M, G[V, E], Option[V]] =
        op { g =>
          val n = Graph[G].neighbourhood(g, vertex)
          n.fold(None: Option[V])(_.out.keySet.find(_ == out))
        }
    }
  }

  import QueryOp._

  /** [[cats.free.Free]] Monad for Graph Queries based upon QueryOp ADT */
  type QueryIO[G[_, _[_]], V, E[_], A] = Free[QueryOp[G, V, E, ?], A]

  /** Syntax enrichment for `QueryIO` */
  implicit class QueryIOOps[G[_, _[_]]: Graph, V, E[_]: Edge, A](fa: QueryIO[G, V, E, A]) {
    def transK[M[_]: Monad]: Kleisli[M, G[V, E], A] = transformK[M, G, V, E](interpretK).apply(fa)

    def transKWith[M[_]: Monad](interp: QueryOp[G, V, E, ?] ~> Kleisli[M, G[V, E], ?]): Kleisli[M, G[V, E], A] = 
      transformK[M, G, V, E](interp).apply(fa)
  }

  /** FoldMaps chosen interpreter over queries constructed from several QueryOp objects (QueryIO) */
  def transformK[M[_]: Monad, G[_, _[_]]: Graph, V, E[_]: Edge]
    (interp: QueryOp[G, V, E, ?] ~> Kleisli[M, G[V, E], ?]): QueryIO[G, V, E, ?] ~> Kleisli[M, G[V, E], ?] =
      new (QueryIO[G, V, E, ?] ~> Kleisli[M, G[V, E], ?]) {
        override def apply[A](fa: QueryIO[G, V, E, A]): Kleisli[M, G[V, E], A] = fa.foldMap(interp)
      }

  /** Default Interpreter which transforms a QuerOp into a Kliesli function which takes a Graph G[V, E] and produces an 
   *  object of the desired result type, wrapped in provided Monad M[?] */
  def interpretK[M[_]: Monad, G[_, _[_]]: Graph, V, E[_]: Edge] = new (QueryOp[G, V, E, ?] ~> Kleisli[M, G[V, E], ?]) {

    def apply[A](fa: QueryOp[G, V, E, A]): Kleisli[M, G[V, E], A] = fa.defaultTransK[M]
  }

  def get[G[_, _[_]], V, E[_]](vertex: V): QueryIO[G, V, E, Option[V]] = liftF[QueryOp[G, V, E, ?], Option[V]](Get(vertex))

  def getWhere[G[_, _[_]], V, E[_]](cond: V => Boolean): QueryIO[G, V, E, List[V]] = liftF[QueryOp[G, V, E, ?], List[V]](GetWhere(cond))

  def traverseEdge[G[_, _[_]], V, E[_]](vertex: V, edge: E[V]): QueryIO[G, V, E, Option[E[V]]] =
    liftF[QueryOp[G, V, E, ?], Option[E[V]]](TraverseEdge(vertex, edge))

  def traverseInNeighbour[G[_, _[_]], V, E[_]](vertex: V, in: V): QueryIO[G, V, E, Option[V]] =
    liftF[QueryOp[G, V, E, ?], Option[V]](TraverseInNeighbour(vertex, in))

  def traverseOutNeighbour[G[_, _[_]], V, E[_]](vertex: V, out: V): QueryIO[G, V, E, Option[V]] =
    liftF[QueryOp[G, V, E, ?], Option[V]](TraverseOutNeighbour(vertex, out))

}
