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
package org.gdget.collection

import cats._
import org.gdget.{Edge, Graph, Neighbourhood}

import scala.language.higherKinds

/** SimpleGraph is an unlabelled, directed graph datastructure. It provides instances of the
  * [[algebra.Monoid]] & [[org.gdget.Graph]] typeclasses.
  * 
  * As its name might suggest, SimpleGraph is neither particularly optimised, or particularly principled (though it is
  * immutable). SimpleGraph is designed to be easy to understand, providing a basis for comparison between various other 
  * Graph implementations, and a development sandbox.
  * 
  * @see [[SimpleNeighbourhood]]
  * @author hugofirth
  */
sealed abstract class SimpleGraph[V] {

  import SimpleGraph._

  private[gdget] def adj: AdjacencyList[V]

  def size: Long
  def order: Long


  def vertices = adj.keys.iterator

  def edges = for {
    (v, neighbours) <- adj.toIterator
    in <- neighbours._1
  } yield (in, v)

}

/** SimpleNeighbourhood is a case class which wraps rows of the Adjacency matrix representation within SimpleGraph.
  * SimpleNeighbourhood instances correspond to the closed neighbourhood of vertices adjacent to a center vertex v; they
  * provide an instance of the [[org.gdget.Neighbourhood]] typeclass.
  *
  * TODO: Update Neighbourhood to have (center, in, out), rather than (center, (in, out))
  *
  * @param center The vertex upon which this neighbourhood is centered
  * @param neighbours The set of vertices which are connected to `center`
  * @tparam V The type of the vertices in the neighbourhood
  */
final case class SimpleNeighbourhood[V](center: V, neighbours: (Set[V], Set[V]))

object SimpleGraph extends SimpleGraphInstances {

  type AdjacencyList[V] = Map[V, (Set[V], Set[V])]

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

  final def apply[V](es: (V, V)*): SimpleGraph[V] = {
    //TODO: Clean up (Neighbours Case class will help) and do in 1 pass
    val repr = es.foldLeft(Map.empty[V, (Set[V], Set[V])]) { (adj,e) =>
      val outAdj = adj.get(e._1).fold(adj + (e._1 -> (Set.empty[V], Set(e._2)))) {
        case (inEdges, outEdges) => adj.updated(e._1, (inEdges, outEdges + e._2))
      }
      outAdj.get(e._2).fold(outAdj + (e._2 -> (Set(e._1), Set.empty[V]))) {
        case (inEdges, outEdges) => outAdj.updated(e._2, (inEdges + e._1, outEdges))
      }
    }
    GCons(repr)
  }

  //TODO: Create conversion methods fromList, fromSet etc.... Maybe extract to a GraphCompanion?

  private[gdget] final case class GCons[V](adj: AdjacencyList[V]) extends SimpleGraph[V] {
    override lazy val size: Long = vertices.size
    override lazy val order: Long = edges.size
  }

  private[gdget] case object NullGraph extends SimpleGraph[Nothing] {
    val size = 0L
    val order = 0L

    override private[gdget] val adj: AdjacencyList[Nothing] = Map.empty[Nothing, (Set[Nothing], Set[Nothing])]

    //TODO: Find out what this unapply is doing (lifted verbatim from Scalaz Map code)
    def unapply[V](g: SimpleGraph[V]): Boolean = g eq this

    def apply[V]: SimpleGraph[V] = this.asInstanceOf[SimpleGraph[V]]
  }
}

trait SimpleGraphInstances {
  import SimpleGraph._

  implicit def simpleGraph[V](implicit eEv: Edge.Aux[(V, V), V],
                              nEv: Neighbourhood[SimpleGraph.N, V, (V, V)]): Graph[SimpleGraph.G, V, (V, V)] =
    new SimpleGraphLike[V] {

      override implicit def E = eEv

      override implicit def N = nEv
    }

  implicit def simpleGraphMonoid[V](implicit ev: Monoid[SimpleGraph.AdjacencyList[V]]): Monoid[SimpleGraph[V]] =
    new Monoid[SimpleGraph[V]] {

      override def empty: SimpleGraph[V] = SimpleGraph.empty[V]

      override def combine(x: SimpleGraph[V], y: SimpleGraph[V]): SimpleGraph[V] = {
        GCons(Monoid[SimpleGraph.AdjacencyList[V]].combine(x.adj, y.adj))
      }
    }

  implicit def tuple2Edge[V0]: Edge[(V0, V0)] = new Edge[(V0, V0)] {

    override type V = V0

    override def vertices(e: (V0, V0)) = e

    override def left(e: (V0, V0)) = e._1

    override def right(e: (V0, V0)) = e._2

    //TODO: Use Cats.Eq
    override def other(e: (V0, V0), v: V0) =
      if(e._1 == v)
        Option(e._2)
      else if(e._2 == v)
        Option(e._1)
      else
        None
  }

  implicit def simpleNeighbourhood[V](implicit eEv: Edge.Aux[(V, V), V]): Neighbourhood[SimpleGraph.N, V, (V, V)] =
   new Neighbourhood[SimpleGraph.N, V, (V, V)] {

     override implicit def E = eEv

     override def edges(n: SimpleNeighbourhood[V]) =
       n.neighbours._1.map((_, n.center)).iterator ++ n.neighbours._2.map((n.center,_)).iterator

     override def center(n: SimpleNeighbourhood[V]) = n.center

   }
}

private[gdget] sealed trait SimpleGraphLike[V] extends Graph[SimpleGraph.G, V, (V, V)] {
  import SimpleGraph._

  override type N[V0, E] = SimpleNeighbourhood[V0]

  //TODO: Use pattern matching to check for NullGraph as a performance optimisation
  //TODO: Investigate Specialization?

  override def size(g: SimpleGraph[V]) = g.size

  override def order(g: SimpleGraph[V]) = g.order

  override def vertices(g: SimpleGraph[V]): Iterator[V] = g.vertices

  override def edges(g: SimpleGraph[V]): Iterator[(V, V)] = g.edges

  override def plusVertex(g: SimpleGraph[V], v: V): SimpleGraph[V] = GCons(g.adj + (v -> (Set.empty[V], Set.empty[V])))

  override def minusVertex(g: SimpleGraph[V], v: V): SimpleGraph[V] = GCons(g.adj - v)

  override def plusEdge(g: SimpleGraph.G[V, (V, V)], e: (V, V)): SimpleGraph.G[V, (V, V)] = {
    //We add to edges._2 because convention for neighbourhood tuples is (inEdges, outEdges), whilst convention for Edge
    //  types is that the left-hand vertex is the source
    val dAdj = g.adj.get(E.left(e)).fold(g.adj + ( E.left(e) -> (Set.empty[V],Set(E.right(e))) )) { edges =>
      g.adj.updated(E.left(e), edges.copy(_2 = edges._2 + E.right(e)))
    }

    //We add to edges._1 because convention for neighbourhood tuples is (inEdges, outEdges), whilst convention for Edge
    //  types is that the right-hand vertex is the destination
    val ddAdj = dAdj.get(E.right(e)).fold(dAdj + (E.right(e) -> (Set(E.left(e)),Set.empty[V]) )) { edges =>
      dAdj.updated(E.right(e), edges.copy(_1 = edges._1 + E.left(e)))
    }
    GCons(ddAdj)
  }

  override def minusEdge(g: SimpleGraph.G[V, (V, V)], e: (V, V)): SimpleGraph.G[V, (V, V)] = {
    //This does check both end of an edge before removing, instead removing as they go. This is fine because removing an
    //  element from a collection where it does not exist returns the same collection.
    val dAdj = g.adj.get(E.left(e)).fold(g.adj)(edges => g.adj.updated(E.left(e), edges.copy(_2 = edges._2 - E.right(e))))
    val ddAdj = dAdj.get(E.right(e)).fold(dAdj)(edges => dAdj.updated(E.left(e), edges.copy(_1 = edges._1 - E.left(e))))
    GCons(ddAdj)
  }

  override def neighbourhood(g: SimpleGraph[V], v: V) = g.adj.get(v).map(SimpleNeighbourhood(v, _))
}

private[gdget] sealed trait SimpleNeighbourhoodLike[V] extends Neighbourhood[SimpleGraph.N, V, (V, V)] {}

private[gdget] sealed trait Tuple2Edge[V] extends Edge[(V, V)] {}

private[gdget] sealed trait SimpleGraphMonoid[V] extends Monoid[SimpleGraph[V]] {}