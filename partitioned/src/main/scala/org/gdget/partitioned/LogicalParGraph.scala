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
import org.gdget.std._
import org.gdget.{Edge, Graph}

import language.higherKinds

/** A logically partitioned graph type. Each vertex is labelled with its partition. */
sealed trait LogicalParGraph[V, E[_]] { self =>
  import LogicalParGraph._

  private[gdget] def adj: AdjacencyList[V]

  private[gdget] implicit def E: Edge[E]

  private[gdget] implicit def V: ParVertex[V]

  def size: Int
  def order: Int
  def numPartitions: Int

  def isEmpty: Boolean

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
  def partitions = adj.groupBy({ case (v, (part, inN, outN)) => part }).valuesIterator.map(GCons(_))

  /** Returns a particular sub adjacency list by index */
  def partition(idx: PartId) = GCons(adj.filter { case (vertex, (part, inN, outN)) => part == idx })

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
        GCons(d2Adj)
      case _ => self //If the vertex does not exist, or the new partition idx is the same as the old: do nothing.
    }
  }
}

object LogicalParGraph extends LogicalParGraphInstances {

  /** Representation of an Directed Adjacency List where each entry is labelled with a partition (wrapped Int) */
  type AdjacencyList[V] = Map[V, (PartId, Map[V, PartId], Map[V, PartId])]

  //TODO: Remove a lot of this boilerplate if possible?
  def empty[V: ParVertex, E[_]: Edge]: LogicalParGraph[V, E] = NullGraph[V, E]


  def apply[V: ParVertex, E[_]: Edge](es: E[V]*) = {
    //TODO: Pass in partitioner so that es traversible is actually partitioned
    // Option 1, make NullGraph take a scheme so that all the code which takes a scheme from a g which may be a NullGraph actually works.
    // Option 2, Remove the need for a scheme and move to the V: ParVertex setup where each vertex added has a partition
    // Option 3 ... ? 
    es.foldLeft(empty[V, E])((g, e) => Graph[LogicalParGraph, V, E].plusEdge(g, e))
  }

  /** Non-empty "Constructor" type of LogicalParGraph */
  private[gdget] final case class GCons[V, E[_]](adj: AdjacencyList[V])
                                                       (implicit val E: Edge[E], val V: ParVertex[V])
    extends LogicalParGraph[V, E] {

      lazy val size: Int = vertices.size
      lazy val order: Int = edges.size
      lazy val numPartitions: Int = partitions.size

      def isEmpty = false
    }

  private[gdget] case object NullGraph extends LogicalParGraph[Nothing, Lambda[A => (A, A)]] {

    val size = 0
    val order = 0
    val numPartitions = 0

    def apply[V: ParVertex, E[_]: Edge]: LogicalParGraph[V, E] = this.asInstanceOf[LogicalParGraph[V, E]]

    def unapply[V: ParVertex, E[_]: Edge](that: LogicalParGraph[V, E]): Boolean = that.isEmpty

    def isEmpty = true

    override private[gdget] implicit def E: Edge[Lambda[A => (A, A)]] = Edge[Lambda[A => (A, A)]]

    //TODO: Check that it is ok form to declare typeclass instances inline like this?
    override private[gdget] implicit def V: ParVertex[Nothing] = new ParVertex[Nothing] {
      override def partition(v: Nothing) = None
    }

    private[gdget] val adj: AdjacencyList[Nothing] = Map.empty[Nothing, (PartId, Map[Nothing, PartId], Map[Nothing, PartId])]
  }
}

//TODO: Investigate why common practice is for these to be sealed abstract classes?
trait LogicalParGraphInstances {

  implicit def logicalParGraph[V, E[_]](implicit eEv: Edge[E], vEv: ParVertex[V]): ParGraph[LogicalParGraph, V, E] =
    new LogicalParGraphLike[V, E] {
      /** Ensure that the type V has a ParVertex typeclass instance */
      override implicit def V: ParVertex[V] = vEv

      override implicit def E: Edge[E] = eEv
    }

}

private[gdget] sealed trait LogicalParGraphLike[V, E[_]] extends ParGraph[LogicalParGraph, V, E] {

  import LogicalParGraph._

  /** Ensure that the type V has a ParVertex typeclass instance */
  override implicit def V: ParVertex[V]

  override implicit def E: Edge[E]

  override def numPartitions(g: LogicalParGraph[V, E]): Int = g.numPartitions

  /** The partitions themselves, index accessible */
  override def partitions(g: LogicalParGraph[V, E]): Vector[LogicalParGraph[V, E]] = g.partitions.toVector

  /** Returns the partition id associated with a specific vertex */
  override def partitionOf(g: LogicalParGraph[V, E], v: V): Option[PartId] = g.partitionOf(v)

  /** Moves a vertex from one partition to another */
  override def updatePartitionOf(g: LogicalParGraph[V, E], v: V, idx: PartId): LogicalParGraph[V, E] = g.updatePartition(v, idx)

  override def point(e: E[V]): LogicalParGraph[V, E] = LogicalParGraph[V, E](e)

  //TODO: Look at using Stream, Streaming or Seq to represent this - Iterator is mutable!
  override def vertices(g: LogicalParGraph[V, E]): Iterator[V] = g.vertices

  override def edges(g: LogicalParGraph[V, E]): Iterator[E[V]] = g.edges

  override def plusEdge(g: LogicalParGraph[V, E], e: E[V]): LogicalParGraph[V, E] = {

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
    val lPart = ParVertex[V].partition(l).getOrElse(0.part)
    val rPart = ParVertex[V].partition(r).getOrElse(0.part)

    //Add r to the outgoing neighbours (l->r convention) of l either in an existing or empty neighbourhood
    val dAdj = lN.fold(g.adj + (l -> (lPart, Map.empty[V, PartId], Map(r -> rPart)))) { case (part, inN, outN) =>
      g.adj + (l -> (part, inN, outN + (r -> rPart)))
    }
    //Add l to the incoming neighbours (l->r convention) of r either in an existing or empty neighbourhood
    val ddAdj = rN.fold(dAdj + (r -> (rPart, Map(l -> lPart), Map.empty[V, PartId]))) { case (part, inN, outN) =>
      dAdj + (r -> (part, inN + (l -> lPart), outN))
    }
    GCons[V, E](ddAdj)
  }

  override def minusEdge(g: LogicalParGraph[V, E], e: E[V]): LogicalParGraph[V, E] = {
    //Get the left hand vertex of Edge e
    val l = Edge[E].left(e)
    //Get the right hand vertex of Edge e
    val r = Edge[E].right(e)
    //Get the neighbourhood for l and remove r from outgoing neighbours. If the neighbourhood does not exist do nothing.
    val dAdj = g.adj.get(l).fold(g.adj) { case (part, inN, outN) => g.adj.updated(l, (part, inN, outN - r)) }
    //Get the nieghbourhood for r and remove l from incoming neighbours. If the neighbourhoos does not exist do nothing.
    val ddAdj = dAdj.get(r).fold(dAdj) { case (part, inN, outN) => g.adj.updated(r, (part, inN - l, outN)) }
    GCons[V, E](ddAdj)
  }

  override def plusVertex(g: LogicalParGraph[V, E], v: V): LogicalParGraph[V, E] = {
    val vPart = ParVertex[V].partition(v).getOrElse(0.part)
    g match {
      case NullGraph() => GCons[V, E](Map(v -> (vPart, Map.empty[V, PartId], Map.empty[V, PartId])))
      case GCons(adj) => GCons[V, E](adj + (v -> adj.getOrElse(v, (vPart, Map.empty[V, PartId], Map.empty[V, PartId]))))
    }
  }

  override def minusVertex(g: LogicalParGraph[V, E], v: V): LogicalParGraph[V, E] = g match {
    case NullGraph() => g
    case GCons(adj) =>
      neighbourhood(g, v) match {
        case Some(n) if g.size <= 1 => NullGraph[V, E]
        case Some(n) => GCons[V, E](this.minusEdges(g, n.edges.toSeq: _*).adj - v)
        case None => g
      }
  }

  override def neighbourhood(g: LogicalParGraph[V, E], v: V): Option[UNeighbourhood[V, E]] = {
    g.adj.get(v).map { case(part, inN, outN) =>
      UNeighbourhood(v, inN.mapValues(id => Set(())), outN.mapValues(id => Set(())))
    }
  }

}
