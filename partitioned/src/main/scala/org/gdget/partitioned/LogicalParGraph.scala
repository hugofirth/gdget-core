/** gdget
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
package org.gdget.partitioned

import org.gdget.data.UNeighbourhood
import org.gdget.partitioned.ParScheme._
import org.gdget.{Edge, Graph}

import language.higherKinds

/** A logically partitioned graph type. Each vertex is labelled with its partition. */
sealed trait LogicalParGraph[S[_], V, E[_]] {

  import LogicalParGraph._

  private[gdget] def adj: AdjacencyList[V]

  private[gdget] def scheme: S[V]

  private[gdget] implicit def E: Edge[E]

  private[gdget] implicit def S: ParScheme[S]

  def size: Int
  def order: Int

  def vertices = adj.keys.iterator

  /** Returns an iterator of E[V], using the connect() constructor of the Edge typeclass to create edges in the right
    * way, only when needed.
    */
  def edges: Iterator[E[V]] = for {
    (v, neighbours) <- adj.toIterator
    (part, inN, outN) = neighbours
    (in, nPart) <- inN
  } yield E.connect(in, v)

  /** Returns an iterator of sub adjancency lists */
  def partitions = adj.groupBy({ case (v, (part, inN, outN)) => part }).valuesIterator.map(GCons(_, scheme))

  /** Returns a particular sub adjacency list by index */
  def partition(idx: PartId) = GCons(adj.filter { case (vertex, (part, inN, outN)) => part == idx }, scheme)

  def partitionOf(v: V) = adj.get(v).map { case (part, inN, outN) => part }

  /** Method to "move" a vertex from one logical partition to another */
  def updatePartition(v: V, idx: PartId) = {
    //Utility method to update the partition of a neighbour
    def updateNeighbourPartition(v: V, n: V, idx: PartId, adj: AdjacencyList[V]) = {
      adj.get(v).fold(adj) { case (part, inN, outN) =>
        val newIn = inN.get(n).fold(inN)(_ => inN.updated(n, idx))
        val newOut = outN.get(n).fold(outN)(_ => outN.updated(n, idx))
        adj.updated(v, (part, newIn, newOut))
      }
    }

    adj.get(v) match {
      case Some((part, inN, outN)) if part != idx =>
        //Update the adjacency list with the new partition index for v
        val dAdj = adj.updated(v, (idx, inN, outN))
        //Update the neighbourhoods for each neighbour of v in the adjacency list
        val d2Adj = (inN ++ outN).foldLeft(dAdj)((acc, entry) => updateNeighbourPartition(entry._1, v, idx, acc))
        GCons(d2Adj, scheme)
      case _ => GCons(adj, scheme) //If the vertex does not exist, or the new partition idx is the same as the old: do nothing.
    }
  }
}

object LogicalParGraph extends LogicalParGraphInstances {

  /** Representation of an Directed Adjacency List where each entry is labelled with a partition (wrapped Int) */
  type AdjacencyList[V] = Map[V, (PartId, Map[V, PartId], Map[V, PartId])]

  def empty[S[_]: ParScheme ,V, E[_]: Edge]: LogicalParGraph[S, V, E] = NullGraph[S, V, E]


  def apply[S[_]: ParScheme, V, E[_]: Edge](scheme: S[V], es: E[V]*) = {
    es.foldLeft(NullGraph[S, V, E])((g, e) => Graph[LogicalParGraph[S, ?, ?[_]]].plusEdge(g, e))
  }

  /** Non-empty "Constructor" type of LogicalParGraph */
  //TODO: Investigate if this is where we should be using Unapply?
  private[gdget] final case class GCons[S[_], V, E[_]](adj: AdjacencyList[V], scheme: S[V])
                                                       (implicit val S: ParScheme[S], val E: Edge[E])
    extends LogicalParGraph[S, V, E] {

      lazy val size: Int = vertices.size
      lazy val order: Int = edges.size
    }

  //TODO: Work out whether we want Lambda[A => Map[Nothing, PartitionId]] or Map[?, PartitionId]. Does it matter?

  private[gdget] case object NullGraph extends LogicalParGraph[Map[?, PartId],
                                                               Nothing, Lambda[A => (Nothing, Nothing)]] {


    /** Adapter type for which there exists an Edge instance (Tuple2[Nothing, Nothing]) */
    type EA[a] = (Nothing, Nothing)

    private[gdget] implicit def E = Edge[EA]

    private[gdget] def scheme: Map[Nothing, PartId] = Map.empty[Nothing, PartId]

    //TODO: Why does ParScheme[Map[?, PartId]] cause ambigious implicits error
    private[gdget] implicit def S = ParScheme.mapInstance

    val size = 0
    val order = 0

    private[gdget] val adj: AdjacencyList[Nothing] =
      Map.empty[Nothing, (PartId, Map[Nothing, PartId], Map[Nothing, PartId])]

    def unapply[S[_]: ParScheme, V, E[_]: Edge](g: LogicalParGraph[S, V, E]): Boolean = g eq this

    def apply[S[_]: ParScheme, V, E[_]: Edge]: LogicalParGraph[S, V, E] = this.asInstanceOf[LogicalParGraph[S, V, E]]
  }
}

//TODO: Investigate why common practice is for these to be sealed abstract classes?
trait LogicalParGraphInstances {

  import LogicalParGraph._

  //TODO: Investigate if here is where we want the Unapply?
  implicit def logicalParGraph[S[_]: ParScheme]: Graph[LogicalParGraph[S, ?, ?[_]]] =
    new Graph[LogicalParGraph[S, ?, ?[_]]] {

      //TODO: Look at using Stream, Streaming or Seq to represent this - Iterator is mutable!
      override def vertices[V, E[_] : Edge](g: LogicalParGraph[S, V, E]): Iterator[V] = g.vertices

      override def edges[V, E[_] : Edge](g: LogicalParGraph[S, V, E]): Iterator[E[V]] = g.edges

      override def plusEdge[V, E[_] : Edge](g: LogicalParGraph[S, V, E], e: E[V]): LogicalParGraph[S, V, E] = {

        //TODO: Reason about the below in the presence of self-loops when you have a moment, suspect broken
        //Get the left hand vertex of Edge e
        val l = Edge[E].left(e)
        //Get the right hand vertex of Edge e
        val r = Edge[E].right(e)
        //Get the neighbourhood of l from g if it exists
        val lN = g.adj.get(l)
        //Get the neighbourhood of r from g if it exists
        val rN = g.adj.get(r)
        //Get the partitions for vertices l & r
        val (dScheme, lPart) = lN.fold(ParScheme[S].getPartition(g.scheme, l, g))(n => (g.scheme, n._1))
        val (ddScheme, rPart) = rN.fold(ParScheme[S].getPartition(dScheme, r, g))(n => (dScheme, n._1))
        //Add r to the outgoing neighbours (l->r convention) of l either in an existing or empty neighbourhood
        val dAdj = lN.fold(g.adj + (l -> (lPart, Map.empty[V, PartId], Map(r -> rPart)))) { case (part, inN, outN) =>
          g.adj + (l -> (part, inN, outN + (r -> rPart)))
        }
        //Add l to the incoming neighbours (l->r convention) of r either in an existing or empty neighbourhood
        val ddAdj = rN.fold(dAdj + (r -> (rPart, Map(l -> lPart), Map.empty[V, PartId]))) { case (part, inN, outN) =>
          dAdj + (r -> (part, inN + (l -> lPart), outN))
        }
        GCons[S, V, E](ddAdj, ddScheme)
      }

      override def minusEdge[V, E[_] : Edge](g: LogicalParGraph[S, V, E], e: E[V]): LogicalParGraph[S, V, E] = {
        //Get the left hand vertex of Edge e
        val l = Edge[E].left(e)
        //Get the right hand vertex of Edge e
        val r = Edge[E].right(e)
        //Get the neighbourhood for l and remove r from outgoing neighbours. If the neighbourhood does not exist do nothing.
        val dAdj = g.adj.get(l).fold(g.adj) { case (part, inN, outN) => g.adj.updated(l, (part, inN, outN - r)) }
        //Get the nieghbourhood for r and remove l from incoming neighbours. If the neighbourhoos does not exist do nothing.
        val ddAdj = dAdj.get(r).fold(dAdj) { case (part, inN, outN) => g.adj.updated(r, (part, inN - l, outN)) }
        GCons[S, V, E](ddAdj, g.scheme)
      }

      override def plusVertex[V, E[_] : Edge](g: LogicalParGraph[S, V, E], v: V): LogicalParGraph[S, V, E] = {
        val (parScheme, part) = ParScheme[S].getPartition(g.scheme, v, g)
        //TODO: Work out why type inferencer gives up on us at this point?
        g match {
          case NullGraph() =>
            GCons[S, V, E](Map(v -> (part, Map.empty[V, PartId], Map.empty[V, PartId])), parScheme)
          case GCons(adj, scheme) =>
            GCons[S, V, E](adj + (v -> adj.getOrElse(v,
              (part, Map.empty[V, PartId], Map.empty[V, PartId]))), parScheme)
        }
      }

      override def minusVertex[V, E[_] : Edge](g: LogicalParGraph[S, V, E], v: V): LogicalParGraph[S, V, E] = g match {
        case NullGraph() => g
        case GCons(adj, scheme) =>
          neighbourhood(g, v) match {
              case Some(n) if g.size <= 1 => NullGraph[S, V, E]
              case Some(n) => GCons[S, V, E](this.minusEdges(g, n.edges.toSeq: _*).adj - v, scheme)
              case None => g
          }
      }

      override def neighbourhood[V, E[_] : Edge](g: LogicalParGraph[S, V, E], v: V): Option[UNeighbourhood[V, E]] = {
        g.adj.get(v).map { case(part, inN, outN) =>
          UNeighbourhood(v, inN.mapValues(id => Set(())), outN.mapValues(id => Set(())))
        }
      }

    }

}

