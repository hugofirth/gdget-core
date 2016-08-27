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
import org.gdget.data.UNeighbourhood

import scala.annotation.implicitNotFound

/** The base Typeclass defining behaviour for simple graphs.
  *
  * @see [[Edge]]
  * @tparam G The type implementing graph like functionality
  * @author hugofirth
  * @since 0.1
  */

@implicitNotFound("No member of type class Graph found for type ${G}")
trait Graph[G[_, _[_]], V, E[_]] extends Any with Serializable {

  implicit def E: Edge[E]

  //TODO: Look at using Stream, Streaming or Seq to represent this - Iterator is mutable!
  def vertices(g: G[V, E]): Iterator[V]
  def edges(g: G[V, E]): Iterator[E[V]]

  /** Constructor method for Graph instances, lifting an edge into a graph. Note that in gdget we consider the edge to 
   *  be the fundamental unit of a graph, not a vertex */
  //TODO: Point/pure is a method on Monad/Applicative in cats to lift a value into a context. As this is what we are
  //  doing here perhaps we need to revisit the idea of Graph as a Monad? Some contention over whether Set is a Monad, 
  //  and one view of a Graph is as a set of edges. 
  def point(e: E[V]): G[V, E]

  /**
    *
    * WARNING: Where appropriate should be overridden in typeclass instances to improve performance (for example if a
    * graph's underlying representation supports O(1) lookups on vertices.
    *
    * @param g
    * @param v
    * @return
    */
  def getVertex(g: G[V, E], v: V): Option[V] = this.findVertex(g)(_ == v)

  def getEdge(g: G[V, E], e: E[V]): Option[E[V]] = this.findEdge(g)(_ == e)

  def neighbourhood(g: G[V, E], v: V): Option[UNeighbourhood[V, E]]

  /** The order of the graph. This is equal to the number of vertices stored.
    *
    * WARNING: Will not complete for graphs where |V| is functionally infinite.
    *
    * @param g
    * @return The number of vertices in the graph
    */
  def order(g: G[V, E]): Long = this.vertices(g).size

  /** The size of the graph. This is equal to the number of edges stored.
    *
    * WARNING: Will not complete for graphs where |E| is functionally infinite.
    *
    * @return The number of edges in the graph
    */
  def size(g: G[V, E]): Long = this.edges(g).size

  def union(lg: G[V, E], rg: G[V, E])(implicit ev: Monoid[G[V,E]]): G[V, E] = Monoid[G[V,E]].combine(lg, rg)

  //TODO: Implement unionAll ?

  def findVertex(g: G[V, E])(f: V => Boolean): Option[V] = this.vertices(g).find(f)

  def findVertices(g: G[V, E])(f: V => Boolean): Iterator[V] = this.vertices(g).filter(f)

  def findEdge(g: G[V, E])(f: E[V] => Boolean): Option[E[V]] = this.edges(g).find(f)

  def findEdges(g: G[V, E])(f: E[V] => Boolean): Iterator[E[V]] = this.edges(g).filter(f)

  def plusVertex(g: G[V, E], v: V): G[V, E]

  def minusVertex(g: G[V, E], v: V): G[V, E]

  def plusVertices(g: G[V, E], vs: V*): G[V, E] = vs.foldLeft(g)((graph, v) => plusVertex(graph, v))

  def minusVertices(g: G[V, E], vs: V*): G[V, E] = vs.foldLeft(g)((graph, v) => minusVertex(graph, v))

  def plusEdge(g: G[V, E], e: E[V]): G[V, E]

  def minusEdge(g: G[V, E], e: E[V]): G[V, E]

  def plusEdges(g: G[V, E], es: E[V]*): G[V, E] = es.foldLeft(g)((graph, e) => plusEdge(graph, e))

  def minusEdges(g: G[V, E], es: E[V]*): G[V, E] = es.foldLeft(g)((graph, e) => minusEdge(graph, e))
}

object Graph {
  
  @inline def apply[G[_, _[_]], V, E[_]](implicit gEv: Graph[G, V, E], eEv: Edge[E]): Graph[G, V, E] = implicitly[Graph[G, V, E]]

  //TODO: Investigate Machinist for cheaper (unboxed) typeclass ops
//  implicit class GraphOps[G[_, _[_]], V, E[_]](val g: G[V, E]) extends AnyVal {
//    def vertices(implicit gEv: Graph[G], eEv: Edge[E]) = gEv.vertices(g)
//    def edges(implicit gEv: Graph[G], eEv: Edge[E]) = gEv.edges(g)
//    def order(implicit gEv: Graph[G], eEv: Edge[E]) = gEv.order(g)
//    def size(implicit gEv: Graph[G], eEv: Edge[E]) = gEv.size(g)
//    def getVertex(v: V)(implicit gEv: Graph[G], eEv: Edge[E]): Option[V] = gEv.getVertex(g, v)
//    def getEdge(e: E[V])(implicit gEv: Graph[G], eEv: Edge[E]): Option[E[V]] = gEv.getEdge[V, E](g, e)
//    def neighbourhood(v: V)(implicit gEv: Graph[G], eEv: Edge[E]) = gEv.neighbourhood(g, v)
//    def union(that: G[V, E])(implicit ev: Monoid[G[V, E]], gEv: Graph[G], eEv: Edge[E]) = gEv.union(g, that)
//    def findVertex(f: (V) => Boolean)(implicit gEv: Graph[G], eEv: Edge[E]) = gEv.findVertex(g)(f)
//    def findEdge(f: (E[V]) => Boolean)(implicit gEv: Graph[G], eEv: Edge[E]) = gEv.findEdge(g)(f)
//    def plusVertex(v: V)(implicit gEv: Graph[G], eEv: Edge[E]) = gEv.plusVertex(g, v)
//    def minusVertex(v: V)(implicit gEv: Graph[G], eEv: Edge[E]) = gEv.minusVertex(g, v)
//    def plusVertices(v: V*)(implicit gEv: Graph[G], eEv: Edge[E]) = gEv.plusVertices(g, v: _*)
//    def minusVertices(v: V*)(implicit gEv: Graph[G], eEv: Edge[E]) = gEv.minusVertices(g, v: _*)
//    def plusEdge(e: E[V])(implicit gEv: Graph[G], eEv: Edge[E]) = gEv.plusEdge(g, e)
//    def minusEdge(e: E[V])(implicit gEv: Graph[G], eEv: Edge[E]) = gEv.minusEdge(g, e)
//    def plusEdges(e: E[V]*)(implicit gEv: Graph[G], eEv: Edge[E]) = gEv.plusEdges(g, e: _*)
//    def minusEdges(e: E[V]*)(implicit gEv: Graph[G], eEv: Edge[E]) = gEv.minusEdges(g, e: _*)
//  }
}

