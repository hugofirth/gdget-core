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

import scala.language.higherKinds

import scalaz._
import Scalaz._


/** The base Typeclass defining behaviour for simple graphs in [[org.gdget.collection.graph]].
  *
  * @see [[VertexLike]]
  * @see [[EdgeLike]]
  * @tparam G The type implementing graph like functionality
  * @tparam V The type implementing vertex like functionality
  * @tparam E The type implementing edge like functionality
  * @author hugofirth
  * @since 0.1
  */
trait GraphLike[G <: AnyRef, V[_], E[_, _, _]] extends Equal[G]{ self =>

  implicit def internalVertex: VertexLike[V, _, E] with InternalElement[V, G]
  implicit def internalEdge: EdgeLike[E, V, _, _, _] with InternalElement[E, G]

  def vertices(g: G): Iterable[V[_]]
  def edges(g: G): Iterable[E[_, _, _]]

  /** The order of the graph. This is equal to the number of vertices stored.
    *
    * WARNING: Will not complete for graphs where |V| is infinite.
    *
    * @return The number of vertices in the graph
    */
  def order(g: G): Int = vertices(g).size

  /** The size of the graph. This is equal to the number of edges stored.
    *
    * WARNING: Will not complete for graphs where |E| is functionally infinite.
    *
    * @return The number of edges in the graph
    */
  def size(g: G): Int = edges(g).size

  def findVertex(g: G)(f: V[_] => Boolean): Option[V[_]]

  def findEdge(g: G)(f: E[_, _, _] => Boolean): Option[E[_, _, _]]

  def plusVertex(g: G, v: V[_]): G

  def minusVertex(g: G, v: V[_]): G

  def plusEdge(g: G, e: E[_, _, _]): G

  def minusEdge(g: G, e: E[_, _, _]): G

  override def equal(g1: G, g2: G) =
    (g1 eq g2) ||
      ((self.order(g1) === self.order(g2)) &&
        (self.size(g1) === self.size(g2)) &&
        (self.vertices(g1) == self.vertices(g2)) &&
        (self.edges(g1) == self.vertices(g2)))

}

object GraphLike {

  implicit class GraphLikeOps[G <: AnyRef, V[_], E[_, _, _]](g: G)(implicit gEv: GraphLike[G, V, E]) {
    def vertices = gEv.vertices(g)
    def edges = gEv.edges(g)
    def order = gEv.order(g)
    def size = gEv.size(g)
    def findVertex(f: V[_] => Boolean) = gEv.findVertex(g)(f)
    def findEdge(f: E[_, _, _] => Boolean) = gEv.findEdge(g)(f)
    def plusVertex(v: V[_]) = gEv.plusVertex(g, v)
    def minusVertex(v: V[_]) = gEv.minusVertex(g, v)
    def plusEdge(e: E[_, _, _]) = gEv.plusEdge(g, e)
    def minusEdge(e: E[_, _, _]) = gEv.minusEdge(g, e)
  }
}


//trait EdgeLabelledGraphLike[G <: AnyRef, V[_], E[_, _, _] <: E[_, Label, _]] extends GraphLike[G, V, E] {
//
//  def find[A](g: G, f: E[_, A, _] => Boolean)(implicit eEv: EdgeLike[E, V, _, A, _]): Option[E[_, A, _]]
//
//}
//
//trait VertexLabelledGraphLike[G <: AnyRef, V[_] <: V[Label], E[_, _, _]] extends GraphLike[G, V, E] {
//
//  def find[A](g: G, f: V[A] => Boolean)(implicit vEv: VertexLike[V, A, E]): Option[V[A]]
//}
//
//trait LabelledGraphLike[G <: AnyRef, V[_] <: V[Label], E[_, _, _] <: E[_, Label, _]]
//  extends GraphLike[G, V, E] with EdgeLabelledGraphLike[G, V, E] with VertexLabelledGraphLike[G, V, E] {
//
//  def find[L, A, R](g: G, f: E[L, A, R] => Boolean)(implicit eEv: EdgeLike[E, V, L, A, R]): Option[E[L, A, R]]
//}

