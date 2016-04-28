/** gdget-core
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
import cats.free.Free.liftF
import org.gdget.{Edge, Graph, LEdge, Path}

/** The ADT representing a basic grammar for read-only queries over collection types which provide [[org.gdget.Graph]]
  * instances.
  *
  * @author hugofirth
  */



sealed trait QueryOp[G[_, _[_]], V, E[_], A]
//TODO: Have getMatch take a Path, not a pattern graph, and have Path have a deconstructor (unapply), so that I can
//  get at individual elements of the path inside the for comprehension of the query.
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