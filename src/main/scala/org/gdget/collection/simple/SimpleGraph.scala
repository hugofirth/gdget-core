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
package org.gdget.collection.simple

import algebra.CommutativeMonoid
import org.gdget.collection._
import org.gdget.{Edge, Graph, Vertex}

/** SimpleGraph is an unlabelled, undirected graph datastructure. It provides instances of the 
  * [[algebra.CommutativeMonoid]] & [[org.gdget.Graph]] typeclasses.
  * 
  * As its name might suggest, SimpleGraph is neither particularly optimised, or particularly functional (though it is
  * immutable). SimpleGraph is designed to be easy to understand, providing a basis for comparison between various other 
  * Graph implementations, and a development sandbox.
  * 
  * TODO: Remove scalaz dependencies...
  *
  * @see [[SimpleEdge]]
  * @author hugofirth
  */
sealed abstract class SimpleGraph[V0, E0] {

  private[simple] val adj: AdjacencyList[V0, E0]
  
  def vertices = adj.keys

  def edges = adj.values.flatten

  def plusVertex(v: V0) = GCons(adj + (v -> Set.empty[E0]))

  def minusVertex(v: V0) = GCons(adj - v)

  def plusVertices(vs: V0*) = vs.foldLeft(this)((g, v) => g plusVertex v)

  def minusVertices(vs: V0*) = vs.foldLeft(this)((g, v) => g minusVertex v)

  def plusEdge(e: E0)(implicit ev: Edge[E0] {type V = V0}) = {
    val dAdj = adj.get(ev.left(e)).fold(adj + (ev.left(e) -> Set(e)))(edges => adj updated(ev.left(e), edges + e))
    val ddAdj = dAdj.get(ev.right(e)).fold(dAdj + (ev.right(e) -> Set(e)))(edges => adj updated(ev.right(e), edges + e))
    GCons(ddAdj)
  }

  def minusEdge(e: E0)(implicit ev: Edge[E0] {type V = V0}) = {
    val dAdj = adj.get(ev.left(e)).fold(adj)(edges => adj updated(ev.left(e), edges - e))
    val ddAdj = dAdj.get(ev.right(e)).fold(dAdj)(edges => dAdj updated(ev.left(e), edges - e))
    GCons(ddAdj)
  }

  def plusEdges(es: E0*)(implicit ev: Edge[E0] {type V = V0}) = es.foldLeft(this)((g, e) => g plusEdge e)

  def minusEdges(es: E0*)(implicit ev: Edge[E0] {type V = V0}) = es.foldLeft(this)((g, e) => g minusEdge e)

}

private[simple] final case class GCons[V0, E0](adj: AdjacencyList[V0, E0]) extends SimpleGraph[V0, E0]

private[simple] case object NullGraph extends SimpleGraph[Nothing, Nothing] {
    val size = 0
    val order = 0

    override private[simple] val adj: AdjacencyList[Nothing, Nothing] = Map.empty[Nothing, Set[Nothing]]

    def unapply[V, E](g: SimpleGraph[V, E]): Boolean = g eq this

    def apply[V, E](): SimpleGraph[V, E] = this.asInstanceOf[SimpleGraph[V, E]]


}

//TODO: Switch to using SimpleEdge in SimpleGraph
//TODO: Look up Aux types to deal with G[_, _] expectation in Graph TC
final case class SimpleEdge[V0](left: V0, right: V0) { type V = V0 }

object SimpleGraph extends SimpleGraphInstances {

  //TODO: Redefine AdjacencyList to be unary in its type parameter

  def empty[V, E]: SimpleGraph[V, E] = NullGraph[V, E]()

  //TODO: Create conversion methods fromList, fromSet etc.... Maybe extract to a GraphCompanion?



}

sealed trait SimpleGraphInstances { self =>

  implicit def simpleGraph[V0, E0](implicit eEv: Edge[E0] {type V = V0},
                                   vEv: Vertex[V0] {type E = E0}): Graph[SimpleGraph, V0, E0] = new SimpleGraphGraph[V0, E0] {


    //TODO: Use a private trait to "once remove" the implicit params from the implicit defs which removes ambiguity.
    // See https://github.com/scalaz/scalaz/search?utf8=%E2%9C%93&q=%22implicit+def%22+path%3Acore%2Fsrc%2Fmain%2Fscala%2Fscalaz%2Fstd%2F&type=Code

    //TODO: Move logic to typeclass instance methods rather than treating them as glue (pending best practices check).
    //TODO: Implement Union and Intersection efficiently.
    override implicit def V: Vertex[V0] = vEv

    override implicit def E: Edge[E0] = eEv
  }

  implicit def simpleGraphMonoid[V: Vertex, E: Edge]: CommutativeMonoid[SimpleGraph[V, E]] =
    new CommutativeMonoid[SimpleGraph[V, E]] {
      override def empty: SimpleGraph[V, E] = NullGraph[V, E]()

      //TODO: Implement combine using Union
      override def combine(x: SimpleGraph[V, E], y: SimpleGraph[V, E]): SimpleGraph[V, E] = ???
    }
}

private trait SimpleGraphGraph[V0, E0] extends Graph[SimpleGraph, V0, E0] {
  override implicit def V: Vertex[V0] {type E = E0}
  override implicit def E: Edge[E0] {type V = V0}

  override def vertices(g: SimpleGraph[V0, E0]): Iterable[V0] = g.vertices

  override def edges(g: SimpleGraph[V0, E0]): Iterable[E0] = g.edges

  override def plusVertex(g: SimpleGraph[V0, E0], v: V0): SimpleGraph[V0, E0] = GCons(g.adj + (v -> Set.empty[E0]))

  override def minusVertex(g: SimpleGraph[V0, E0], v: V0): SimpleGraph[V0, E0] = GCons(g.adj - v)

  override def plusEdge(g: SimpleGraph[V0, E0], e: E0): SimpleGraph[V0, E0] = g plusEdge e

  override def minusEdge(g: SimpleGraph[V0, E0], e: E0): SimpleGraph[V0, E0] = g minusEdge e

  override def union(lg: SimpleGraph[V0, E0], rg: SimpleGraph[V0, E0]): SimpleGraph[V0, E0] = ???

  override def intersection(lg: SimpleGraph[V0, E0], rg: SimpleGraph[V0, E0]): SimpleGraph[V0, E0] = ???
}
