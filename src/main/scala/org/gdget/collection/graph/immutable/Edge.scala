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
package org.gdget.collection.graph.immutable

import org.gdget.collection.graph._

import language.higherKinds

sealed trait Edge[L, +A, R] {
  type Vertex[_]
  implicit def vertexLikeV: VertexLike[Vertex, _, Edge]
  def left: Vertex[L]
  def right: Vertex[R]
  def value: A
}

/** This case class implements the the [[org.gdget.collection.graph.EdgeLike]] type class.
  *
  * This is the most basic (undirected) edge type in [[org.gdget.collection.graph]] and also the default implementation
  * returned from the `Edge(...)` factory in [[org.gdget.collection.graph.Edge]].
  *
  * The left ~~ right naming convention for edge vertices used throughout [[org.gdget.collection.graph]] does not
  * necessarily imply any bias or preferred direction.
  *
  *          _.-=-._     .-,
  *        .'       "-.,' /
  *       (          _.  <
  *        `=.____.="  `._\
  *
  * @param left The left hand vertex connected by an edge
  * @param value The value being stored in an edge
  * @param right The right hand vertex connected by an edge
  * @tparam L The type of the left hand vertex connected by an edge
  * @tparam A The type of the value being stored in an edge
  * @tparam R The type of the right hand vertex connected by an edge
  */
case class OuterEdge[L, A, R, V[_]] (left: V[L], value: A, right: V[R])(implicit vEv: ExternalVertexLike[V, _, OuterEdge])
  extends Edge[L, A, R] {

  override type Vertex[_] = V

  override implicit def vertexLikeV: ExternalVertexLike[V, _, OuterEdge] = vEv
}

case class InnerEdge[L, A, R, V[_, _], G] (left: V[L, G], value: A, right: V[R, G])
                                          (implicit vEv: InternalVertexLike[V, _, OuterEdge, G],
                                           gEv: GraphLike[G, V, InnerEdge]) extends Edge[L, A, R] {

  override type Vertex[_] = V

  override implicit def vertexLikeV: InternalVertexLike[V, _, OuterEdge, G] = vEv
}

object Edge {

  implicit def edgeLikeOuterEdge[L, A, R, V[_]](implicit vEv: ExternalVertexLike[V, _, OuterEdge]): ExternalEdgeLike[OuterEdge, V, L, A, R] =
    new ExternalEdgeLike[OuterEdge, V, L, A, R] {

      override def get(elem: OuterEdge[L, A, R, V]): A = elem.value

      override type Inner[G] = InnerEdge[L, A, R, V, G]

      override def toInner[G](elem: OuterEdge[L, A, R, V], graph: G)
                             (implicit ev: InternalElement[Inner[G], A, G], gEv: GraphLike[G, _, _]): Inner[G] = ???

      override implicit def vertexLikeV: ExternalVertexLike[V, _, OuterEdge] = vEv

      override def vertices(e: OuterEdge[L, A, R, V]): (V[L], V[R]) = (e.left, e.right)

      override def left(e: OuterEdge[L, A, R, V]): V[L] = e.left

      override def right(e: OuterEdge[L, A, R, V]): V[R] = e.right

      override def other(e: OuterEdge[L, A, R, V], v: V[_]): Option[V[_]] = {
        if(vEv.get(v) === vEv.get(e.left)) {
          Some(e.right)
        } else if(vEv.get(v) === vEv.get(e.right)) {
          Some(e.left)
        } else {
          None
        }
      }
    }

  implicit def edgeLikeInnerEdge[L, A, R, V[_], G]: InternalEdgeLike[InnerEdge, V, L, A, R, G] =
    new InternalEdgeLike[InnerEdge, V, L, A, R, G] {
      override implicit def vertexLikeV: InternalVertexLike[V, _, InnerEdge, G] = ???

      override def graph(elem: InnerEdge[L, A, R, V, G])(implicit gEv: GraphLike[G, _, _]): G = ???

      override def toOuter(elem: InnerEdge[L, A, R, V, G])(implicit ev: ExternalElement[Outer, A]): Outer = ???

      override type Outer = OuterVertex

      override def get(elem: InnerEdge[L, A, R, V, G]): A = ???

      override def vertices(e: InnerEdge): (V[L], V[R]) = ???

      override def left(e: InnerEdge): V[L] = ???

      override def right(e: InnerEdge): V[R] = ???

      override def other(e: InnerEdge, v: V[_]): Option[V[_]] = ???
    }

}


