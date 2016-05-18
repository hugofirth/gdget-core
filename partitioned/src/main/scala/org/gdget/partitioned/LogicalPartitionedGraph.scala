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
import org.gdget.partitioned.PartitionScheme.PartitionId
import org.gdget.{Edge, Graph}

import language.higherKinds

/** A logically partitioned graph type. Each vertex is labelled with its partition. */
sealed trait LogicalPartitionedGraph[V, E[_]] {

  import LogicalPartitionedGraph._

  /** Type for this graph's partitioning scheme */
  type S[_]

  private[gdget] def adj: AdjacencyList[V]

  private[gdget] def scheme: S[V]

  private[gdget] implicit def E: Edge[E]

  private[gdget] implicit def S: PartitionScheme[S]

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

object LogicalPartitionedGraph extends LogicalPartitionedGraphInstances {

  /** Representation of an Directed Adjacency List where each entry is labelled with a partition (wrapped Int) */
  type AdjacencyList[V] = Map[V, (PartitionId, Map[V, PartitionId], Map[V, PartitionId])]

  def empty[V, E[_]: Edge]: LogicalPartitionedGraph[V, E] = NullGraph[V, E]

  def apply[V, E[_]: Edge, P[_]: Partitioner](partitioner: P[LogicalPartitionedGraph[V, E]], es: E[V]*) = ???

  /** Non-empty "Constructor" type of LogicallyPartitionedGraph */
  private[gdget] final case class GCons[V, E[_], S[_]](adj: AdjacencyList[V], scheme: S[V])(implicit val S: PartitionScheme[S], val E: Edge[E])
    extends LogicalPartitionedGraph[V, E] {

      lazy val size: Int = vertices.size
      lazy val order: Int = edges.size
  }

  private[gdget] case object NullGraph extends LogicalPartitionedGraph[Nothing, Lambda[A => (Nothing, Nothing)]] {

    /** Adapter type for which there exists an Edge instance (Tuple2[Nothing, Nothing]) */
    type EA[a] = (Nothing, Nothing)

    private[gdget] implicit def E = Edge[EA]

    /** Adapter type for which there exists a PartitionScheme instance Map[Nothing, Int] */
    type S[a] = Map[Nothing, PartitionId]

    private[gdget] def scheme: Map[Nothing, PartitionId] = Map.empty[Nothing, PartitionId]

    private[gdget] implicit def S = PartitionScheme[S]

    val size = 0
    val order = 0

    private[gdget] val adj: AdjacencyList[Nothing] =
      Map.empty[Nothing, (PartitionId, Map[Nothing, PartitionId], Map[Nothing, PartitionId])]

    def unapply[V, E[_]: Edge](g: LogicalPartitionedGraph[V, E]): Boolean = g eq this

    def apply[V, E[_]: Edge]: LogicalPartitionedGraph[V, E] = this.asInstanceOf[LogicalPartitionedGraph[V, E]]
  }
}


trait LogicalPartitionedGraphInstances {

  import LogicalPartitionedGraph._

  implicit def logicalPartitionedGraph: Graph[LogicalPartitionedGraph] = new Graph[LogicalPartitionedGraph] {
    override def vertices[V, E[_] : Edge](g: LogicalPartitionedGraph[V, E]): Iterator[V] = g.vertices

    override def edges[V, E[_] : Edge](g: LogicalPartitionedGraph[V, E]): Iterator[E[V]] = g.edges

    override def plusVertex[V, E[_] : Edge](g: LogicalPartitionedGraph[V, E], v: V): LogicalPartitionedGraph[V, E] = {
      val part = g.partitionOf(v)
      //TODO: work out why on earth we have to explicitly pass in the PartitionScheme instance explicitly.
      g match {
        case NullGraph() =>
          GCons(Map(v -> (part, Map.empty[V, PartitionId], Map.empty[V, PartitionId])), g.scheme)(g.S, g.E)
        case GCons(adj, scheme) =>
          GCons(adj + (v -> adj.getOrElse(v, (part, Map.empty[V, PartitionId], Map.empty[V, PartitionId]))), g.scheme)(g.S, g.E)
      }
    }

    override def minusVertex[V, E[_] : Edge](g: LogicalPartitionedGraph[V, E], v: V): LogicalPartitionedGraph[V, E] = ???

    override def plusEdge[V, E[_] : Edge](g: LogicalPartitionedGraph[V, E], e: E[V]): LogicalPartitionedGraph[V, E] = ???

    override def minusEdge[V, E[_] : Edge](g: LogicalPartitionedGraph[V, E], e: E[V]): LogicalPartitionedGraph[V, E] = ???

    override def neighbourhood[V, E[_] : Edge](g: LogicalPartitionedGraph[V, E], v: V): Option[UNeighbourhood[V, E]] = ???

  }

}
