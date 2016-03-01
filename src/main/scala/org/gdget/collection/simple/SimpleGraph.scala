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
import org.gdget.{Edge, Graph, Vertex}

/** SimpleGraph is an unlabelled, undirected graph datastructure. It provides instances of the 
  * [[algebra.CommutativeMonoid]] & [[org.gdget.Graph]] typeclasses.
  * 
  * As its name might suggest, SimpleGraph is neither particularly optimised, or particularly principled (though it is
  * immutable). SimpleGraph is designed to be easy to understand, providing a basis for comparison between various other 
  * Graph implementations, and a development sandbox.
  * 
  * @see [[SimpleVertex]]
  * @author hugofirth
  */
sealed abstract class SimpleGraph[V0] {

  private[simple] def adj: AdjacencyList[V0]

  def size: Long
  def order: Long
  
  def vertices = adj.keys.iterator

  def edges = for {
    (v, neighbours) <- adj.toIterator
    n <- neighbours
  } yield (v, n)

}

private[simple] final case class GCons[V0](adj: AdjacencyList[V0]) extends SimpleGraph[V0] {
  override lazy val size: Long = vertices.size
  override lazy val order: Long = edges.size
}

private[simple] case object NullGraph extends SimpleGraph[Nothing] {
    val size = 0
    val order = 0

    override private[simple] val adj: AdjacencyList[Nothing] = Map.empty[Nothing, Set[Nothing]]

    def unapply[V](g: SimpleGraph[V]): Boolean = g eq this

    def apply[V]: SimpleGraph[V] = this.asInstanceOf[SimpleGraph[V]]
}

/** SimpleVertex is a case class which wraps rows of the Adjacency matrix stored within SimpleGraph. SimpleVertex are
  * never stored, only existing when the underlying vertex representation objects (V) are being added or removed from a
  * graph.
  *
  * [[SimpleGraphInstances]] provides an instance of the [[org.gdget.Vertex]] typeclass for SimpleVertex.
  *
  * TODO: Decide between implicit G and Graph[G] params for all Vertex TC functions, or rethinking the top level Graph API
  *
  * Don't mind assuming there will always be a VWrapper (VertexView, VContext w.e) but how do we handle HK types in Graph TC?
  *
  * Edge return types are also tricky ... Does the edge store VWrappers? Maybe the edge constructor takes an implicit G?
  *
  * TODO: Add getVertex method to Graph
  *
  * @param v The vertex's underlying representation
  * @param neighbours The set of vertex representations which are connected to v
  * @tparam V The type of the vertex's underlying representation
  */
final case class SimpleVertex[V](v: V, neighbours: Set[V])

object SimpleGraph extends SimpleGraphInstances {

  /** Alias allows SimpleGraph to "pretend" to conform to expected graph type-structure G[V,E], though E always equals (V, V) */
  type G[V, E] = SimpleGraph[V]

  //TODO: Look at ScalaGraph for shared companion objects
  //TODO: Look at Cats and decide which typeclasses all Graphs should provide instances for?
  //TODO: Provide Edge instance for Tuple2
  //TODO: Switch to using SimpleVertex - rename SimpleVertexView?


  def empty[V]: SimpleGraph[V] = NullGraph[V]

  //TODO: Create final apply method for creating SimpleGraphs w. any implicits fot TC[V] where TC are required typlecasses

  //TODO: Create conversion methods fromList, fromSet etc.... Maybe extract to a GraphCompanion?
}

sealed trait SimpleGraphInstances { self =>

  implicit def simpleGraph[V0](implicit eEv: Edge[(V0, V0)] {type V = V0},
                               vEv: Vertex[V0] {type E = (V0, V0)}): Graph[SimpleGraph.G, V0, (V0, V0)] =
    new SimpleGraphGraph[V0] {

      override implicit def V: Vertex[V0] = vEv

      override implicit def E: Edge[(V0, V0)] = eEv
    }

  implicit def simpleGraphMonoid[V: Vertex, E: Edge]: CommutativeMonoid[SimpleGraph[V]] =
    new CommutativeMonoid[SimpleGraph[V]] {
      override def empty: SimpleGraph[V] = NullGraph[V]

      //TODO: Efficiently implement combine.
      override def combine(x: SimpleGraph[V], y: SimpleGraph[V]): SimpleGraph[V] = ???
    }

  implicit def tuple2Edge[V0: Vertex]: Edge[(V0, V0)] = new Edge[(V0, V0)] {

    override type V = V0
    override implicit def vertexV = implicitly[Vertex[V0]]

    override def vertices(e: (V0, V0)) = e

    override def left(e: (V0, V0)) = e._1

    override def right(e: (V0, V0)) = e._2

    override def other(e: (V0, V0), v: V0) =
      if(e._1 == v)
        Option(e._2)
      else if(e._2 == v)
        Option(e._1)
      else None
  }
}

private trait SimpleGraphGraph[V0] extends Graph[SimpleGraph.G, V0, (V0, V0)] {
  override implicit def V: Vertex.Aux[V0, (V0, V0)]
  override implicit def E: Edge.Aux[(V0, V0), V0]

  //TODO: Move implementations of plus/minus methods back up to main type?
  //TODO: Use pattern matching to check for NullGraph as a performance optimisation
  //TODO: Investigate Specialization?

  override def size(g: SimpleGraph[V0]) = g.size

  override def order(g: SimpleGraph[V0]) = g.order

  override def vertices(g: SimpleGraph[V0]): Iterator[V0] = g.vertices

  override def edges(g: SimpleGraph[V0]): Iterator[(V0, V0)] = g.edges

  override def plusVertex(g: SimpleGraph[V0], v: V0): SimpleGraph[V0] = GCons(g.adj + (v -> Set.empty[V0]))

  override def minusVertex(g: SimpleGraph[V0], v: V0): SimpleGraph[V0] = GCons(g.adj - v)

  override def plusEdge(g: SimpleGraph.G[V0, (V0, V0)], e: (V0, V0)): SimpleGraph.G[V0, (V0, V0)] = {
    val dAdj = g.adj.get(E.left(e)).fold(g.adj + (E.left(e) -> Set(E.right(e)))) { edges =>
      g.adj.updated(E.left(e), edges + E.right(e))
    }
    val ddAdj = dAdj.get(E.right(e)).fold(dAdj + (E.right(e) -> Set(E.left(e)))) { edges =>
      dAdj updated(E.right(e), edges + E.left(e))
    }
    GCons(ddAdj)
  }

  override def minusEdge(g: SimpleGraph.G[V0, (V0, V0)], e: (V0, V0)): SimpleGraph.G[V0, (V0, V0)] = {
    val dAdj = g.adj.get(E.left(e)).fold(g.adj)(edges => g.adj.updated(E.left(e), edges - E.right(e)))
    val ddAdj = dAdj.get(E.right(e)).fold(dAdj)(edges => dAdj.updated(E.left(e), edges - E.left(e)))
    GCons(ddAdj)
  }
}
