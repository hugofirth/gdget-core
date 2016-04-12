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

import cats._

/** The base Typeclass defining behaviour for simple graphs.
  *
  * @see [[Edge]]
  * @tparam G The type implementing graph like functionality
  * @author hugofirth
  * @since 0.1
  */
trait Graph[G[_, _]] extends Any with Serializable {


  /** type member N represents the closed-neighbourhood of a given vertex v, and should provide a [[Neighbourhood]] instance */
  type N[_, _]

  /** Make sure that E and N have instances of the appropriate typeclasses */
//  implicit def E: Edge[E]
//  implicit def N: Neighbourhood[N, V, E]

  //TODO: Look at using Stream, Streaming or Seq to represent this - Iterator is mutable!
  def vertices[V, E[_]: Edge](g: G[V, E[V]]): Iterator[V]
  def edges[V, E[_]: Edge](g: G[V, E[V]]): Iterator[E[V]]

  /**
    *
    * WARNING: Where appropriate should be overridden in typeclass instances to improve performance (for example if a
    * graph's underlying representation supports O(1) lookups on vertices.
    *
    * @param g
    * @param v
    * @return
    */
  def getVertex[V, E[_]: Edge](g: G[V, E[V]], v: V): Option[V] = this.findVertex(g)(_ == v)

  def getEdge[V, E[_]: Edge](g: G[V, E[V]], e: E[V]): Option[E[V]] = this.findEdge(g)(_ == e)

  def neighbourhood[V, E[_]: Edge](g: G[V, E[V]], v: V): Option[N[V, E[V]]]

  /** The order of the graph. This is equal to the number of vertices stored.
    *
    * WARNING: Will not complete for graphs where |V| is functionally infinite.
    *
    * @param g
    * @return The number of vertices in the graph
    */
  def order[V, E[_]: Edge](g: G[V, E[V]]): Long = this.vertices(g).size

  /** The size of the graph. This is equal to the number of edges stored.
    *
    * WARNING: Will not complete for graphs where |E| is functionally infinite.
    *
    * @return The number of edges in the graph
    */
  def size[V, E[_]: Edge](g: G[V, E[V]]): Long = this.edges(g).size

  def union[V, E[_]: Edge](lg: G[V, E[V]], rg: G[V, E[V]])(implicit ev: Monoid[G[V,E[V]]]): G[V, E[V]] = 
    Monoid[G[V,E[V]]].combine(lg, rg)

  //TODO: Implement unionAll ?

  def findVertex[V, E[_]: Edge](g: G[V, E[V]])(f: (V) => Boolean): Option[V] = this.vertices(g) find f

  def findEdge[V, E[_]: Edge](g: G[V, E[V]])(f: (E[V]) => Boolean): Option[E[V]] = this.edges(g) find f

  def plusVertex[V, E[_]: Edge](g: G[V, E[V]], v: V): G[V, E[V]]

  def minusVertex[V, E[_]: Edge](g: G[V, E[V]], v: V): G[V, E[V]]

  def plusVertices[V, E[_]: Edge](g: G[V, E[V]], vs: V*): G[V, E[V]] = vs.foldLeft(g)((graph, v) => plusVertex(graph, v))

  def minusVertices[V, E[_]: Edge](g: G[V, E[V]], vs: V*): G[V, E[V]] = vs.foldLeft(g)((graph, v) => minusVertex(graph, v))

  def plusEdge[V, E[_]: Edge](g: G[V, E[V]], e: E[V]): G[V, E[V]]

  def minusEdge[V, E[_]: Edge](g: G[V, E[V]], e: E[V]): G[V, E[V]]

  def plusEdges[V, E[_]: Edge](g: G[V, E[V]], es: E[V]*): G[V, E[V]] = es.foldLeft(g)((graph, e) => plusEdge(graph, e))

  def minusEdges[V, E[_]: Edge](g: G[V, E[V]], es: E[V]*): G[V, E[V]] = es.foldLeft(g)((graph, e) => minusEdge(graph, e))
}

object Graph {
  
  @inline def apply[G[_, _]: Graph]: Graph[G] = implicitly[Graph[G]]

  //TODO: Investigate Machinist for cheap (unboxed) typeclass ops. Or else extend AnyVal
  //Possible alternative to machinist is to have Ops classes not take implicit gEv and have it extend AnyVal.
  implicit class GraphOps[G[_, _]: Graph, V, E[_]: Edge](g: G[V, E[V]]) {
    def vertices = Graph[G].vertices(g)
    def edges = Graph[G].edges(g)
    def order = Graph[G].order(g)
    def size = Graph[G].size(g)
    def getVertex(v: V): Option[V] = Graph[G].getVertex(g, v)
    def getEdge(e: E[V]): Option[E[V]] = Graph[G].getEdge[V, E](g, e)
    def neighbourhood(v: V) = Graph[G].neighbourhood(g, v)
    def union(that: G[V, E[V]])(implicit ev: Monoid[G[V, E[V]]]) = Graph[G].union(g, that)
    def findVertex(f: (V) => Boolean) = Graph[G].findVertex(g)(f)
    def findEdge(f: (E[V]) => Boolean) = Graph[G].findEdge(g)(f)
    def plusVertex(v: V) = Graph[G].plusVertex(g, v)
    def minusVertex(v: V) = Graph[G].minusVertex(g, v)
    def plusVertices(v: V*) = Graph[G].plusVertices(g, v: _*)
    def minusVertices(v: V*) = Graph[G].minusVertices(g, v: _*)
    def plusEdge(e: E[V]) = Graph[G].plusEdge(g, e)
    def minusEdge(e: E[V]) = Graph[G].minusEdge(g, e)
    def plusEdges(e: E[V]*) = Graph[G].plusEdges(g, e: _*)
    def minusEdges(e: E[V]*) = Graph[G].minusEdges(g, e: _*)
  }
}

