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
package org.gdget.data

import cats._
import org.gdget._

import scala.language.{higherKinds, reflectiveCalls}

/** SimpleGraph is an unlabelled, directed graph datastructure. It provides instances of the
  * [[algebra.Monoid]] & [[org.gdget.Graph]] typeclasses.
  * 
  * As its name might suggest, SimpleGraph is neither particularly optimised, or particularly principled (though it is
  * immutable). SimpleGraph is designed to be easy to understand, providing a basis for comparison between various other 
  * Graph implementations, and a development sandbox.
  * 
  * @author hugofirth
  */
sealed trait SimpleGraph[V, E[_]] {

  import SimpleGraph._

  private[gdget] def adj: AdjacencyList[V]

  private[gdget] implicit def E: Edge[E]

  def size: Int
  def order: Int

  def vertices = adj.keys.iterator

  def edges = for {
    (v, neighbours) <- adj.toIterator
    in <- neighbours._1
  } yield E.connect(in, v)

}

object SimpleGraph extends SimpleGraphInstances {

  type AdjacencyList[V] = Map[V, (Set[V], Set[V])]


  //TODO: Look at ScalaGraph for shared companion objects
  //TODO: Look at Cats and decide which typeclasses all Graphs should provide instances for?

  def empty[V, E[_]: Edge]: SimpleGraph[V, E] = NullGraph[V, E]

  def apply[V, E[_]: Edge](es: E[V]*): SimpleGraph[V, E] = {
    //TODO: Clean up (Neighbours Case class will help) and do in 1 pass
    val repr = es.foldLeft(Map.empty[V, (Set[V], Set[V])]) { (adj,e) =>
      val outAdj = adj.get(Edge[E].left(e)).fold(adj + (Edge[E].left(e) -> (Set.empty[V], Set(Edge[E].right(e))))) {
        case (inEdges, outEdges) => adj.updated(Edge[E].left(e), (inEdges, outEdges + Edge[E].right(e)))
      }
      outAdj.get(Edge[E].right(e)).fold(outAdj + (Edge[E].right(e) -> (Set(Edge[E].left(e)), Set.empty[V]))) {
        case (inEdges, outEdges) => outAdj.updated(Edge[E].right(e), (inEdges + Edge[E].left(e), outEdges))
      }
    }
    GCons(repr)
  }

  //TODO: Create conversion methods fromList, fromSet etc.... Maybe extract to a GraphCompanion?

  private[gdget] final case class GCons[V, E[_]](adj: AdjacencyList[V])(implicit val E: Edge[E]) extends SimpleGraph[V, E] {

    lazy val size = vertices.size
    lazy val order = edges.size
  }

  private[gdget] case object NullGraph extends SimpleGraph[Nothing, Lambda[A => (Nothing, Nothing)]] {

    type EA[a] = (Nothing, Nothing)

    private[gdget] implicit def E = Edge[EA]

    val size = 0
    val order = 0

    private[gdget] val adj: AdjacencyList[Nothing] = Map.empty[Nothing, (Set[Nothing], Set[Nothing])]

    def unapply[V, E[_]: Edge](g: SimpleGraph[V, E]): Boolean = g eq this

    def apply[V, E[_]: Edge]: SimpleGraph[V, E] = this.asInstanceOf[SimpleGraph[V, E]]
  }
}

trait SimpleGraphInstances {

  import SimpleGraph._

  implicit def simpleGraph: Graph[SimpleGraph] =
    new SimpleGraphLike {

    }

  implicit def simpleGraphMonoid[V, E[_] : Edge](implicit ev: Monoid[SimpleGraph.AdjacencyList[V]]): Monoid[SimpleGraph[V, E]] =
    new Monoid[SimpleGraph[V, E]] {

      override def empty: SimpleGraph[V, E] = SimpleGraph.empty[V, E]

      override def combine(x: SimpleGraph[V, E], y: SimpleGraph[V, E]): SimpleGraph[V, E] = {
        GCons(Monoid[SimpleGraph.AdjacencyList[V]].combine(x.adj, y.adj))
      }
    }


}
private[gdget] sealed trait SimpleGraphLike extends Graph[SimpleGraph] {

  import SimpleGraph._

  //TODO: Use pattern matching to check for NullGraph as a performance optimisation
  //TODO: Investigate Specialization?
  //TODO: Look at using Stream, Streaming or Seq to represent this - Iterator is mutable!
  override def vertices[V, E[_] : Edge](g: SimpleGraph[V, E]): Iterator[V] = g.vertices

  override def edges[V, E[_] : Edge](g: SimpleGraph[V, E]): Iterator[E[V]] = g.edges

  override def plusVertex[V, E[_] : Edge](g: SimpleGraph[V, E], v: V): SimpleGraph[V, E] = {
    //TODO: Make Singleton Graph type
    g match {
      case NullGraph() => GCons(Map(v -> (Set.empty[V], Set.empty[V])))
      case GCons(adj) => GCons(adj + (v -> adj.getOrElse(v, (Set.empty[V], Set.empty[V]))))
    }
  }

  override def minusVertex[V, E[_] : Edge](g: SimpleGraph[V, E], v: V): SimpleGraph[V, E] = {
    g match {
      case NullGraph() => NullGraph[V, E]
      case GCons(adj) if g.size <= 1 => NullGraph[V, E]
      case GCons(adj) => GCons(adj - v)
    }
  }

  override def plusEdge[V, E[_] : Edge](g: SimpleGraph[V, E], e: E[V]): SimpleGraph[V, E] = {
    //We add to edges._2 because convention for neighbourhood tuples is (inEdges, outEdges), whilst convention for Edge
    //  types is that the left-hand vertex is the source
    val dAdj = g.adj.get(Edge[E].left(e)).fold(g.adj + ( Edge[E].left(e) -> (Set.empty[V],Set(Edge[E].right(e))) )) { edges =>
      g.adj + (Edge[E].left(e) -> edges.copy(_2 = edges._2 + Edge[E].right(e)))
    }

    //We add to edges._1 because convention for neighbourhood tuples is (inEdges, outEdges), whilst convention for Edge
    //  types is that the right-hand vertex is the destination
    val ddAdj = dAdj.get(Edge[E].right(e)).fold(dAdj + (Edge[E].right(e) -> (Set(Edge[E].left(e)),Set.empty[V]) )) { edges =>
      dAdj.updated(Edge[E].right(e), edges.copy(_1 = edges._1 + Edge[E].left(e)))
    }
    GCons(ddAdj)
  }

  override def minusEdge[V, E[_] : Edge](g: SimpleGraph[V, E], e: E[V]): SimpleGraph[V, E] = {
    //This does check both end of an edge before removing, instead removing as they go. This is fine because removing an
    //  element from a collection where it does not exist returns the same collection.
    val dAdj = g.adj.get(Edge[E].left(e)).fold(g.adj) { edges =>
      g.adj.updated(Edge[E].left(e), edges.copy(_2 = edges._2 - Edge[E].right(e)))
    }
    val ddAdj = dAdj.get(Edge[E].right(e)).fold(dAdj) { edges =>
      dAdj.updated(Edge[E].left(e), edges.copy(_1 = edges._1 - Edge[E].left(e)))
    }
    GCons(ddAdj)
  }

  override def neighbourhood[V, E[_] : Edge](g: SimpleGraph[V, E], v: V) = g.adj.get(v).map { case (in, out) =>
      val mapper = (acc: Map[V, Set[Unit]], elem: V) => acc + (elem -> Set(()))
      UNeighbourhood(v, in.foldLeft(Map.empty[V, Set[Unit]])(mapper), out.foldLeft(Map.empty[V, Set[Unit]])(mapper))
  }

}

