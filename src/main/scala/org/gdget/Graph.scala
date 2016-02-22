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
package org.gdget

import scala.language.{existentials, higherKinds, reflectiveCalls}

/** The base Typeclass defining behaviour for simple graphs.
  *
  * Note that my implicit conversions from tinker/neo/graphx to gdget are infact just typeclass instances.
  *
  * @see [[Vertex]]
  * @see [[Edge]]
  * @tparam G The type implementing graph like functionality
  * @tparam V The vertex type contained in a graph of type G
  * @tparam E the edge type connecting vertices of type V, in a graph of type G
  * @author hugofirth
  * @since 0.1
  */
trait Graph[G[_, _], V, E] { self =>

  /** Make sure that V, E have instances of appropriate typeclasses */
  implicit def V: Vertex[V]
  implicit def E: Edge[E]

  def vertices(g: G[V, E]): Iterable[V]
  def edges(g: G[V, E]): Iterable[E]


  /** The order of the graph. This is equal to the number of vertices stored.
    *
    * WARNING: Will not complete for graphs where |V| is functionally infinite.
    *
    * @return The number of vertices in the graph
    */
  def order(g: G[V, E]): Int = self.vertices(g).size

  /** The size of the graph. This is equal to the number of edges stored.
    *
    * WARNING: Will not complete for graphs where |E| is functionally infinite.
    *
    * @return The number of edges in the graph
    */
  def size(g: G[V, E]): Int = self.edges(g).size

  def union(lg: G[V, E], rg: G[V, E]): G[V, E]

  def intersection(lg: G[V, E], rg: G[V, E]): G[V, E]

  def findVertex(g: G[V, E])(f: (V) => Boolean): Option[V] = self.vertices(g) find f

  def findEdge(g: G[V, E])(f: (E) => Boolean): Option[E] = self.edges(g) find f

  def plusVertex(g: G[V, E], v: V): G[V, E]

  def minusVertex(g: G[V, E], v: V): G[V, E]

  def plusVertices(g: G[V, E], vs: V*): G[V, E] = vs.foldLeft(g)((graph, v) => plusVertex(graph, v))

  def minusVertices(g: G[V, E], vs: V*): G[V, E] = vs.foldLeft(g)((graph, v) => minusVertex(graph, v))

  def plusEdge(g: G[V, E], e: E): G[V, E]

  def minusEdge(g: G[V, E], e: E): G[V, E]

  def plusEdges(g: G[V, E], es: E*): G[V, E] = es.foldLeft(g)((graph, e) => plusEdge(graph, e))

  def minusEdges(g: G[V, E], es: E*): G[V, E] = es.foldLeft(g)((graph, e) => minusEdge(graph, e))
}

object Graph {

  implicit class GraphOps[G[_, _], V: Vertex, E: Edge](g: G[V, E])(implicit gEv: Graph[G, V, E]) {
    def vertices = gEv.vertices(g)
    def edges = gEv.edges(g)
    def order = gEv.order(g)
    def size = gEv.size(g)
    def union(that: G[V, E]) = gEv.union(g, that)
    def intersection(that: G[V, E]) = gEv.intersection(g, that)
    def findVertex(f: (V) => Boolean) = gEv.findVertex(g)(f)
    def findEdge(f: (E) => Boolean) = gEv.findEdge(g)(f)
    def plusVertex(v: V) = gEv.plusVertex(g, v)
    def minusVertex(v: V) = gEv.minusVertex(g, v)
    def plusVertices(v: V*) = gEv.plusVertices(g, v: _*)
    def minusVertices(v: V*) = gEv.minusVertices(g, v: _*)
    def plusEdge(e: E) = gEv.plusEdge(g, e)
    def minusEdge(e: E) = gEv.minusEdge(g, e)
    def plusEdges(e: E*) = gEv.plusEdges(g, e: _*)
    def minusEdges(e: E*) = gEv.minusEdges(g, e: _*)
  }
}

