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

import language.higherKinds
import algebra.CommutativeMonoid
import org.gdget.{Neighbourhood, Edge, Graph}

/** SimpleGraph is an unlabelled, undirected graph datastructure. It provides instances of the 
  * [[algebra.CommutativeMonoid]] & [[org.gdget.Graph]] typeclasses.
  * 
  * As its name might suggest, SimpleGraph is neither particularly optimised, or particularly principled (though it is
  * immutable). SimpleGraph is designed to be easy to understand, providing a basis for comparison between various other 
  * Graph implementations, and a development sandbox.
  * 
  * @see [[SimpleNeighbourhood]]
  * @author hugofirth
  */
sealed abstract class SimpleGraph[V] {

  private[simple] def adj: AdjacencyList[V]

  def size: Long
  def order: Long
  
  def vertices = adj.keys.iterator

  def edges = for {
    (v, neighbours) <- adj.toIterator
    n <- neighbours
  } yield (v, n)

}

private[simple] final case class GCons[V](adj: AdjacencyList[V]) extends SimpleGraph[V] {
  override lazy val size: Long = vertices.size
  override lazy val order: Long = edges.size
}

private[simple] case object NullGraph extends SimpleGraph[Nothing] {
    val size = 0L
    val order = 0L

    override private[simple] val adj: AdjacencyList[Nothing] = Map.empty[Nothing, Set[Nothing]]

    def unapply[V](g: SimpleGraph[V]): Boolean = g eq this

    def apply[V]: SimpleGraph[V] = this.asInstanceOf[SimpleGraph[V]]
}

/** SimpleNeighbourhood is a case class which wraps rows of the Adjacency matrix representation within SimpleGraph.
  * SimpleNeighbourhood instances correspond to the closed neighbourhood of vertices adjacent to a center vertex v; they
  * provide an instance of the [[org.gdget.Neighbourhood]] typeclass.
  *
  * @param center The vertex upon which this neighbourhood is centered
  * @param neighbours The set of vertices which are connected to `center`
  * @tparam V The type of the vertices in the neighbourhood
  */
final case class SimpleNeighbourhood[V](center: V, neighbours: Set[V])

object SimpleGraph extends SimpleGraphInstances {

  /** Aliases allow SimpleGraph to "pretend" to conform to structure expected in Graph and Neighbourhood typeclasses
    * despite E being fixed as (V, V) */
  //TODO: look into correct type bounds on E, because at the moment E is just dropped / ignored - which is icky
  //Re the above ^ type bounds like this make the compiler have a panic attack because they are stricter bounds than are
  // declared in the Neighbourhood and Graph typeclasses. TODO: fix the bounds in Neighbourhood and Graph typeclasses.
  type G[V, E] = SimpleGraph[V]
  type N[V, E] = SimpleNeighbourhood[V]

  //TODO: Look at ScalaGraph for shared companion objects
  //TODO: Look at Cats and decide which typeclasses all Graphs should provide instances for?

  def empty[V]: SimpleGraph[V] = NullGraph[V]

  //TODO: Create final apply method for creating SimpleGraphs w. any implicits fot TC[V] where TC are required typlecasses

  //TODO: Create conversion methods fromList, fromSet etc.... Maybe extract to a GraphCompanion?
}

sealed trait SimpleGraphInstances { self =>

  implicit def simpleGraph[V](implicit eEv: Edge.Aux[(V, V), V]): Graph[SimpleGraph.G, V, (V, V)] =
    new SimpleGraphGraph[V] {

      override implicit def E = eEv

      override implicit def N: Neighbourhood[N, V, (V, V)] = ???
    }

  implicit def simpleGraphMonoid[V]: CommutativeMonoid[SimpleGraph[V]] =
    new CommutativeMonoid[SimpleGraph[V]] {
      override def empty: SimpleGraph[V] = NullGraph[V]

      //TODO: Efficiently implement combine.
      override def combine(x: SimpleGraph[V], y: SimpleGraph[V]): SimpleGraph[V] = ???
    }

  implicit def tuple2Edge[V0]: Edge[(V0, V0)] = new Edge[(V0, V0)] {

    override type V = V0

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

  implicit def simpleNeighbourhood[V](implicit eEv: Edge.Aux[(V, V), V]): Neighbourhood[SimpleGraph.N, V, (V, V)] =
   new Neighbourhood[SimpleGraph.N, V, (V, V)] {

     override implicit def E = eEv

     override def edges(n: SimpleNeighbourhood[V]) = n.neighbours.map((n.center, _)).iterator

     override def center(n: SimpleNeighbourhood[V]) = n.center

   }
}

private[simple] trait SimpleGraphGraph[V] extends Graph[SimpleGraph.G, V, (V, V)] {

  override type N[V0, E] = SimpleNeighbourhood[V0]

  //TODO: Move implementations of plus/minus methods back up to main type?
  //TODO: Use pattern matching to check for NullGraph as a performance optimisation
  //TODO: Investigate Specialization?

  override def size(g: SimpleGraph[V]) = g.size

  override def order(g: SimpleGraph[V]) = g.order

  override def vertices(g: SimpleGraph[V]): Iterator[V] = g.vertices

  override def edges(g: SimpleGraph[V]): Iterator[(V, V)] = g.edges

  override def plusVertex(g: SimpleGraph[V], v: V): SimpleGraph[V] = GCons(g.adj + (v -> Set.empty[V]))

  override def minusVertex(g: SimpleGraph[V], v: V): SimpleGraph[V] = GCons(g.adj - v)

  override def plusEdge(g: SimpleGraph.G[V, (V, V)], e: (V, V)): SimpleGraph.G[V, (V, V)] = {
    val dAdj = g.adj.get(E.left(e)).fold(g.adj + (E.left(e) -> Set(E.right(e)))) { edges =>
      g.adj.updated(E.left(e), edges + E.right(e))
    }
    val ddAdj = dAdj.get(E.right(e)).fold(dAdj + (E.right(e) -> Set(E.left(e)))) { edges =>
      dAdj updated(E.right(e), edges + E.left(e))
    }
    GCons(ddAdj)
  }

  override def minusEdge(g: SimpleGraph.G[V, (V, V)], e: (V, V)): SimpleGraph.G[V, (V, V)] = {
    val dAdj = g.adj.get(E.left(e)).fold(g.adj)(edges => g.adj.updated(E.left(e), edges - E.right(e)))
    val ddAdj = dAdj.get(E.right(e)).fold(dAdj)(edges => dAdj.updated(E.left(e), edges - E.left(e)))
    GCons(ddAdj)
  }

  override def neighbourhood(g: SimpleGraph[V], v: V) = g.adj.get(v).map(SimpleNeighbourhood(v, _))
}
