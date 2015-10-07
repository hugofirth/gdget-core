/** gdget-core
  *
  * Copyright (c) 2015 Hugo Firth
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
package org.gdget.collection.graph

import language.{higherKinds, reflectiveCalls}
import scalaz._
import Scalaz._

/** The base TypeClass for defining behaviour for a vertex
  *
  * @see [[EdgeLike]]
  * @tparam V The type implementing vertex like functionality
  * @tparam A The type of value to be represented by the vertex
  * @author hugofirth
  * @since 0.1
  */
sealed trait VertexLike[V[_], A, +E] { self =>

  implicit def edgeLikeE[Edge >: E]: EdgeLike[Edge, V, _, _, _]

  def edges[Edge >: E](v: V[A]): Set[Edge]

  def plusEdge[Edge >: E](v: V[A], e: Edge): V

  def minusEdge[Edge >: E](v: V[A], e: Edge): V

  def neighbours[N[_] >: V[_]](v: V[A]): Set[N[_]]

  def degree(v: V[A]): Int = self.edges(v).size


}

trait ExternalVertexLike[V[_], A, E[_, _, _, _]] extends VertexLike[V[A], A, E[_, _, _, V]] with ExternalElement[V, A] { self =>

  override implicit def edgeLikeE[Edge >: E]: ExternalEdgeLike[Edge, V, _, _, _]

  def isContainedIn(v: V[A], other: V[A]): Boolean = (self.get(v) === self.get(other)) &&
    self.edges(v).subsetOf(self.edges(other))
}


trait InternalVertexLike[V[_, _], A, E[_, _, _, _, _], G]
  extends VertexLike[V[A, G], A, E[_, _, _, _, _]] with InternalElement[({type λ[a] = V[a, G]})#λ, A, G] {  self =>

  override implicit def edgeLikeE[Edge >: E]: InternalEdgeLike[Edge, V, _, _, _, G]

  def isContainedIn(v: V[A, G], other: V[A, G]): Boolean = (self.get(v) === self.get(other)) &&
    self.edges(v).subsetOf(self.edges(other))
}

object VertexLike {

  implicit class VertexLikeOps[V[_], A, E](v: V[A])(implicit eEv: EdgeLike[E, V, _, _, _],
                                                             vEv: VertexLike[V, A, E]) {
    def get: A = vEv.get(v)
    def edges: Set[E] = vEv.edges(v)
    def plusEdge(e: E): V = vEv.plusEdge(v, e)
    def minusEdge(e: E): V = vEv.minusEdge(v, e)
    def neighbours[N[_] >: V[_]]: Set[N[_]] = vEv.neighbours(v)
    def degree: Int = vEv.degree(v)
    def isContainedIn(other: V[A]): Boolean = vEv.isContainedIn(v, other)
  }
}

