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

import org.gdget.collection.graph.Vertex.VertexOps
import org.gdget.util._

import scala.language.{higherKinds, existentials}

import scalaz._
import Scalaz._
import scalaz.std.iterable.iterableEqual

/** The base Typeclass defining behaviour for simple graphs in [[org.gdget.collection.graph]].
  *
  * @see [[Vertex]]
  * @see [[Edge]]
  * @tparam G The type implementing graph like functionality
  * @author hugofirth
  * @since 0.1
  */
trait Graph[G <: AnyRef] extends Equal[G]{ self =>

//  def vertices(g: G): Iterable[(V, Vertex[V]) forSome { type V }]
  def vertices(g: G): Iterable[(V Having Vertex) forSome { type V }]
  def edges(g: G): Iterable[(E[_, _], Edge[E]) forSome { type E[_,_] }]

  /** The order of the graph. This is equal to the number of vertices stored.
    *
    * WARNING: Will not complete for graphs where |V| is functionally infinite.
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

  def findVertex[V : Vertex](g: G)(f: V => Boolean): Option[V]

  def findEdge[E[_, _], L, R](g: G)(f: E[L, R] => Boolean)(implicit eEv: Edge[E]): Option[E[L, R]]

  def plusVertex[V : Vertex](g: G, v: V): G

  def minusVertex[V : Vertex](g: G, v: V): G

  def plusEdge[E[_, _], L, R](g: G, e: E[L, R])(implicit eEv: Edge[E]): G

  def minusEdge[E[_, _], L, R](g: G, e: E[L, R])(implicit eEv: Edge[E]): G

  override def equal(g1: G, g2: G) = {

    (g1 eq g2) ||
      ((self.order(g1) === self.order(g2)) &&
        (self.size(g1) === self.size(g2)) &&
        (self.vertices(g1) == self.vertices(g2)) &&
        (self.edges(g1) == self.edges(g2)))
  }

  def test(g: G) = vertices(g).map { case v Has ev => ev.degree(v) }
  // vertices(g).map { case e Has ev: Vertex[V] => ev.degree(v) } is that better?

  def a: Having[Int, Vertex]
  def b: Having[Int, Vertex]
  def c = a === b

}

object Graph {



  implicit class GraphOps[G <: AnyRef](g: G)(implicit gEv: Graph[G]) {
    def vertices = gEv.vertices(g)
    def edges = gEv.edges(g)
    def order = gEv.order(g)
    def size = gEv.size(g)
    def findVertex[V : Vertex](f: V => Boolean) = gEv.findVertex(g)(f)
    def findEdge[E[_, _], L, R](f: E[L, R] => Boolean)(implicit eEv: Edge[E]) = gEv.findEdge(g)(f)
    def plusVertex[V : Vertex](v: V) = gEv.plusVertex(g, v)
    def minusVertex[V : Vertex](v: V) = gEv.minusVertex(g, v)
    def plusEdge[E[_, _], L, R](e: E[L, R])(implicit eEv: Edge[E]) = gEv.plusEdge(g, e)
    def minusEdge[E[_, _], L, R](e: E[L, R])(implicit eEv: Edge[E]) = gEv.minusEdge(g, e)
  }
}

