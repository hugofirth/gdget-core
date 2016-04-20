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
package org.gdget

import language.higherKinds

/** The Neighbourhood typeclass represents the closed neighbourhood of a given center vertex v; i.e. the induced
  * sub-graph of a graph G which includes v and all v's adjacent vertices.
  *
  * NOTE: Although N is a graph in the mathematical sense, it does not exhibit a strict superset of the Graph typeclass'
  * functionality (unlike Monoid and Semigroup for instance) therefore Neighbourhood does not extend Graph.
  *
  * @tparam N The type implementing neighbourhood-like functionality
  * @see [[Graph]]
  * @author hugofirth
  */
trait Neighbourhood[N[_, _, _]] extends Any with Serializable { self =>

  /** Make sure that E provides an instance of the Edge typeclass with type member V = V */
  def degree[V, E[_, +_], L](n: N[V, E[V, L], L])(implicit ev: LabelledEdge[E, L]): Int = self.edges(n).size

  def center[V, E[_, +_], L](n: N[V, E[V, L], L])(implicit ev: LabelledEdge[E, L]): V

  //TODO: look at pattern matching constructor for edge (center |->| v, v |->| center etc...) to increase clarity of this method
  //Should override for performance if neighbours are known directly, otherwise we're going neighbours->edges->neighbours
  def neighbours[V, E[_, +_], L](n: N[V, E[V, L], L])(implicit ev: LabelledEdge[E, L]): Iterator[V] =
    edges(n).map(LabelledEdge[E, L].other(_, self.center(n))).flatten


  def edges[V, E[_, +_], L](n: N[V, E[V, L], L])(implicit ev: LabelledEdge[E, L]): Iterator[E[V, L]]

  /** Method for returning the "In" edges adjacent to the center vertex of a given neighbourhood.
    *
    * NOTE: Remember that gdget's conventions regarding edge directions are as follows:
    *   - All edges are directed by default.
    *   - The direction of an edge may be ignored at query time to simulate the existence of an undirected edge.
    *   - The simple convention is that an edge's left vertex is the source, its right vertex the destination.
    *
    * @param n The neighbourhood whose directed "In" edges we are looking to return
    * @return The set of edges whose destination vertex equals center(n)
    */
  def inEdges[V, E[_, +_], L](n: N[V, E[V, L], L])(implicit ev: LabelledEdge[E, L]): Iterator[E[V, L]] =
    self.edges(n).filter(LabelledEdge[E, L].right(_) == self.center(n))



  /** Method for returning the "Out" edges adjacent to the center vertex of a given neighbourhood.
    *
    * NOTE: Remember that gdget's conventions regarding edges are as follows:
    *   - All edges are directed by default.
    *   - The direction of an edge may be ignored at query time to simulate the existence of an undirected edge.
    *   - The simple convention is that an edge's left vertex is the source, its right vertex the destination.
    *
    * @param n The neighbourhood whose directed "out" edges we are looking to return
    * @return The set of edges whose source vertex equals center(n)
    */
  def outEdges[V, E[_, +_], L](n: N[V, E[V, L], L])(implicit ev: LabelledEdge[E, L]): Iterator[E[V, L]] =
    self.edges(n).filter(LabelledEdge[E, L].left(_) == self.center(n))

}

object Neighbourhood {

  @inline def apply[N[_, _, _]: Neighbourhood]: Neighbourhood[N] = implicitly[Neighbourhood[N]]

  implicit class NeighbourhoodOps[N[_, _, _], V, E[_, +_], L](val n: N[V, E[V, L], L]) extends AnyVal {
    def edges(implicit nEv: Neighbourhood[N], eEv: LabelledEdge[E, L]) = nEv.edges(n)
    def neighbours(implicit nEv: Neighbourhood[N], lEv: LabelledEdge[E, L]) = nEv.neighbours(n)
    def degree(implicit nEv: Neighbourhood[N]) = nEv.degree(n)
  }
}
