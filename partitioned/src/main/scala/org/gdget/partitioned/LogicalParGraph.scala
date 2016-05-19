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
import org.gdget.partitioned.ParScheme.PartitionId
import org.gdget.{Edge, Graph}

import language.higherKinds

/** A logically partitioned graph type. Each vertex is labelled with its partition. */
sealed trait LogicalParGraph[V, E[_]] {

  import LogicalParGraph._

  /** Type for this graph's partitioning scheme */
  type S[_]

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
  def partitions = adj.groupBy { case (v, (part, inN, outN)) => part }.valuesIterator.map(GCons(_, scheme))

  /** Returns a particular sub adjacency list by index */
  def partition(idx: PartitionId) = GCons(adj.filter { case (vertex, (part, inN, outN)) => part == idx }, scheme)

  def partitionOf(v: V) = S.getPartition(scheme, v)

  /** Method to "move" a vertex from one logical partition to another */
  def updatePartition(v: V, idx: PartitionId) = {
    //Utility method to update the partition of a neighbour
    def updateNeighbourPartition(v: V, n: V, idx: PartitionId, adj: AdjacencyList[V]) = {
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

  /** Aux type alias to simplify including S type refinements for LogicalParGraphs */
  type Aux[V, E[_], S0[_]] = LogicalParGraph[V, E] { type S[a] = S0[a] }

  /** Representation of an Directed Adjacency List where each entry is labelled with a partition (wrapped Int) */
  type AdjacencyList[V] = Map[V, (PartitionId, Map[V, PartitionId], Map[V, PartitionId])]

  def empty[V, E[_]: Edge]: LogicalParGraph[V, E] = NullGraph[V, E]

  def apply[V, E[_]: Edge, P[_]: Partitioner](partitioner: P[LogicalParGraph[V, E]], es: E[V]*) = ???

  /** Non-empty "Constructor" type of LogicalParGraph */
  private[gdget] final case class GCons[V, E[_], S0[_]](adj: AdjacencyList[V], scheme: S0[V])
                                                       (implicit val S: ParScheme[S0], val E: Edge[E])
    extends LogicalParGraph[V, E] {

      override type S[a] = S0[a]

      lazy val size: Int = vertices.size
      lazy val order: Int = edges.size
    }

  private[gdget] case object NullGraph extends LogicalParGraph[Nothing, Lambda[A => (Nothing, Nothing)]] {

    /** Adapter type for which there exists an Edge instance (Tuple2[Nothing, Nothing]) */
    type EA[a] = (Nothing, Nothing)

    private[gdget] implicit def E = Edge[EA]

    /** Adapter type for which there exists a PartitionScheme instance Map[Nothing, Int] */
    type S[a] = Map[Nothing, PartitionId]

    private[gdget] def scheme: Map[Nothing, PartitionId] = Map.empty[Nothing, PartitionId]

    private[gdget] implicit def S = ParScheme[S]

    val size = 0
    val order = 0

    private[gdget] val adj: AdjacencyList[Nothing] =
      Map.empty[Nothing, (PartitionId, Map[Nothing, PartitionId], Map[Nothing, PartitionId])]

    def unapply[V, E[_]: Edge](g: LogicalParGraph[V, E]): Boolean = g eq this

    def apply[V, E[_]: Edge]: LogicalParGraph[V, E] = this.asInstanceOf[LogicalParGraph[V, E]]
  }
}


trait LogicalParGraphInstances {

  import LogicalParGraph._

  implicit def logicalPartitionedGraph[S[_]: ParScheme]: Graph[Lambda[(A, B[_]) => LogicalParGraph.Aux[A, B, S]]] =
    new Graph[Lambda[(A, B[_]) => LogicalParGraph.Aux[A, B, S]]] {

      //TODO: Look at using Stream, Streaming or Seq to represent this - Iterator is mutable!
      override def vertices[V, E[_] : Edge](g: Aux[V, E, S]): Iterator[V] = g.vertices

      override def edges[V, E[_] : Edge](g: Aux[V, E, S]): Iterator[E[V]] = g.edges

      override def plusEdge[V, E[_] : Edge](g: Aux[V, E, S], e: E[V]): Aux[V, E, S] = {

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
        val lPart = lN.fold(ParScheme[S].getPartition(g.scheme, l))(_._1)
        val rPart = rN.fold(ParScheme[S].getPartition(g.scheme, r))(_._1)
        //Add r to the outgoing neighbours (l->r convention) of l either in an existing or empty neighbourhood
        val dAdj = lN.fold(g.adj + (l -> (lPart, Map.empty[V, PartitionId], Map(r -> rPart)))) { case (part, inN, outN) =>
          g.adj + (l -> (part, inN, outN + (r -> rPart)))
        }
        //Add l to the incoming neighbours (l->r convention) of r either in an existing or empty neighbourhood
        val ddAdj = rN.fold(dAdj + (r -> (rPart, Map(l -> lPart), Map.empty[V, PartitionId]))) { case (part, inN, outN) =>
          dAdj + (r -> (part, inN + (l ->lPart), outN))
        }
        GCons[V, E, S](ddAdj, g.scheme)
      }

      override def minusEdge[V, E[_] : Edge](g: Aux[V, E, S], e: E[V]): Aux[V, E, S] = {
        //Get the left hand vertex of Edge e
        val l = Edge[E].left(e)
        //Get the right hand vertex of Edge e
        val r = Edge[E].right(e)
        //Get the neighbourhood for l and remove r from outgoing neighbours. If the neighbourhood does not exist do nothing.
        val dAdj = g.adj.get(l).fold(g.adj) { case (part, inN, outN) => g.adj.updated(l, (part, inN, outN - r)) }
        //Get the nieghbourhood for r and remove l from incoming neighbours. If the neighbourhoos does not exist do nothing.
        val ddAdj = dAdj.get(r).fold(dAdj) { case (part, inN, outN) => g.adj.updated(r, (part, inN - l, outN)) }
        GCons[V, E, S](ddAdj, g.scheme)
      }

      override def plusVertex[V, E[_] : Edge](g: Aux[V, E, S], v: V): Aux[V, E, S] = {
        val part = g.partitionOf(v)
        //TODO: Work out why type inferencer gives up on us at this point?
        g match {
          case NullGraph() =>
            GCons[V, E, S](Map(v -> (part, Map.empty[V, PartitionId], Map.empty[V, PartitionId])), g.scheme)
          case GCons(adj, scheme) =>
            GCons[V, E, S](adj + (v -> adj.getOrElse(v, (part, Map.empty[V, PartitionId], Map.empty[V, PartitionId]))), g.scheme)
        }
      }

      override def minusVertex[V, E[_] : Edge](g: Aux[V, E, S], v: V): Aux[V, E, S] = ???

      override def neighbourhood[V, E[_] : Edge](g: Aux[V, E, S], v: V): Option[UNeighbourhood[V, E]] = ???



  }

}
