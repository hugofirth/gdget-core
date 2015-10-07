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


/** The base TypeClass for defining behaviour for all edges stored in a graph.
  *
  * Types which implement EdgeLike are considered to be unbaised, undirected edges.
  *
  * @see [[VertexLike]]
  * @tparam E The type implementing edge like functionality
  * @tparam V The vertex type to be `linked`
  * @tparam L The type contained in the vertex at the left-hand end of the edge
  * @tparam R The type contained in the vertex at the right-hand end of the edge
  * @author hugofirth
  * @since 0.1
  */
sealed trait EdgeLike[E[_, _, _, _], V[_], L, A, R] {

  implicit def vertexLikeV: VertexLike[V, _, E]

  def vertices(e: E[L, A, R, V]): (V[L], V[R])

  def other(e: E[L, A, R, V], v: V[_]): Option[V[_]]

  def left(e: E[L, A, R, V]): V[L]

  def right(e: E[L, A, R, V]): V[R]
}

trait ExternalEdgeLike[E[_, +_, _, _], V[_], L, A, R]
  extends EdgeLike[E[L, A, R, V], V, L, A, R] with ExternalElement[({type λ[a] = E[L, a, R, V]})#λ, A] {

  override implicit def vertexLikeV: ExternalVertexLike[V, _, E]
}

trait InternalEdgeLike[E[_, +_, _, _, _], V[_, _], L, A, R, G]
  extends EdgeLike[E[L, A, R, V, G], V, L, A, R] with InternalElement[({type λ[a] = E[L, a, R, V, G]})#λ, A, G] {

  override implicit def vertexLikeV: InternalVertexLike[V, _, E, G]
}

object EdgeLike {

  implicit class EdgeLikeOps[E[_, _, _, _], V[_], L, A, R](e: E[L, A, R, V])(implicit eEv: EdgeLike[E, V, L, A, R], vEv: VertexLike[V, _, E]) {
    def vertices = eEv.vertices(e)
    def left = eEv.left(e)
    def right = eEv.right(e)
    def other(v: V[_])(implicit vEv:  VertexLike[V, _, E]): Option[V[_]] = eEv.other(e, v)
  }

  implicit class ExternalEdgeLikeOps[E[_, _, _, _], V[_], L, A, R](e: E[L, A, R, V])
                                                                  (implicit eEv: ExternalEdgeLike[E, V, L, A, R]) {
    def get = eEv.get(e)
  }

//  implicit class InternalEdgeLikeOps[E[_, _, _, _, _], V[_], L, A, R, G](e: E[L, A, R, V, G])
}


//trait EdgeBuilder[B] { self =>
//
//  def from[V[_], L, Bl[_[_]]](from: V[L], builder: B)(implicit vEv: VertexLike[V], bEv: PartialEdgeBuilderLeft[Bl]): Bl[V[L]]
//  def to[V[_], R, Br[_[_]]](to: V[R], builder: B)(implicit vEv: VertexLike[V], bEv: PartialEdgeBuilderRight[Br]): Br[V[R]]
//  def between[V[_], L, R, Bb[_[_], _[_]]](vLeft: V[L], vRight: V[R], builder: B)(implicit vEv: VertexLike[V], bEv: PartialEdgeBuilderBoth[Bb]): Bb[V[L], V[R]]
//  def fromTo()
//}
//
//trait PartialEdgeBuilderLeft[B[V[L]]] { self =>
//
//  def to[V[_], L, R, Bb[_[_], _[_]]](to: V[R], builder: B[V[L]])(implicit vEv: VertexLike[V], bEv: PartialEdgeBuilderBoth[Bb]): Bb[V[L], V[R]]
//}
//
//trait PartialEdgeBuilderRight[B[V[R]]] { self =>
//
//  def from[V[_], L, R, Bb[_[_], _[_]]](from: V[L], builder: B[V[R]])(implicit vEv: VertexLike[V], bEv: PartialEdgeBuilderBoth[Bb]): Bb[V[L], V[R]]
//}
//
//trait PartialEdgeBuilderBoth[B[_[L], _[R]]] { self =>
//
//  def containing[V[_], L, A, R, Cb[_[_], _, _[_]]](value: A, builder: B[V[L], V[R]])(implicit vEv: VertexLike[V], bEv: CompleteEdgeBuilder[Cb]): Cb[V[L], A, V[R]]
//}
//
//trait CompleteEdgeBuilder[B[_[L], A, _[R]]] { self =>
//
//  def get[E[_[_], _, _[_]], V[_], L, A, R](builder: B[V[L], A, V[R]])(implicit vEv: VertexLike[V], eEv: EdgeLike[E]): E[V[L], A, V[R]]
//}


