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
sealed trait LogicalParGraph[S[_], V, E[_]] { self =>
  import LogicalParGraph._

  private[gdget] def adj: AdjacencyList[V]

  private[gdget] def scheme: S[V]

  private[gdget] implicit def E: Edge[E]

  private[gdget] implicit def S: ParScheme[S]

  def size: Int
  def order: Int

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
      case _ => self //If the vertex does not exist, or the new partition idx is the same as the old: do nothing.
    }
  }
}

object LogicalParGraph extends LogicalParGraphInstances {

  /** Representation of an Directed Adjacency List where each entry is labelled with a partition (wrapped Int) */
  type AdjacencyList[V] = Map[V, (PartId, Map[V, PartId], Map[V, PartId])]

  def empty[S[_]: ParScheme ,V, E[_]: Edge](scheme: S[V]): LogicalParGraph[S, V, E] = NullGraph[S, V, E](scheme)


  def apply[S[_]: ParScheme, V, E[_]: Edge](scheme: S[V], es: E[V]*) = {
    //TODO: Pass in the scheme so that the partitions are actually different.
    // Option 1, make NullGraph take a scheme so that all the code which takes a scheme from a g which may be a NullGraph actually works.
    // Option 2, Remove the need for a scheme and move to the V: ParVertex setup where each vertex added has a partition
    // Option 3 ... ? 
    es.foldLeft(empty[S, V, E](scheme))((g, e) => Graph[LogicalParGraph[S, ?, ?[_]]].plusEdge(g, e))
  }

  /** Overloaded apply method to construct LogicalParGraph with a default instance of type S[V] */
  def apply[S[_]: ParScheme, V, E[_]: Edge](es: E[V]*) = {
    es.foldLeft(empty[S, V, E](ParScheme[S].default[V]))((g, e) => Graph[LogicalParGraph[S, ?, ?[_]]].plusEdge(g, e))
  }

  /** Non-empty "Constructor" type of LogicalParGraph */
  //TODO: Investigate if this is where we should be using Unapply?
  private[gdget] final case class GCons[S[_], V, E[_]](adj: AdjacencyList[V], scheme: S[V])
                                                       (implicit val S: ParScheme[S], val E: Edge[E])
    extends LogicalParGraph[S, V, E] {

      lazy val size: Int = vertices.size
      lazy val order: Int = edges.size

      def isEmpty = false
    }

  //TODO: Work out whether we want Lambda[A => Map[Nothing, PartitionId]] or Map[?, PartitionId]. Does it matter?

  private[gdget] case class NullGraph[S[_], V, E[_]](scheme: S[V])(implicit val S: ParScheme[S], val E: Edge[E]) 
    extends LogicalParGraph[S, V, E] {

    val size = 0
    val order = 0

    def isEmpty = true 

    private[gdget] val adj: AdjacencyList[V] = Map.empty[V, (PartId, Map[V, PartId], Map[V, PartId])]

  }
}

//TODO: Investigate why common practice is for these to be sealed abstract classes?
trait LogicalParGraphInstances {

  import LogicalParGraph._

  //TODO: Investigate if here is where we want the Unapply?
  implicit def logicalParGraph[S[_]: ParScheme]: ParGraph[LogicalParGraph[S, ?, ?[_]]] =
    new ParGraph[LogicalParGraph[S, ?, ?[_]]] {

      private def assign[V, E[_]: Edge](v: V, g: LogicalParGraph[S, V, E], p: PartId, scheme: S[V]) = g match {
        //TODO: Work out why type inferencer gives up on us at this point?
        case NullGraph(_) =>
          GCons[S, V, E](Map(v -> (p, Map.empty[V, PartId], Map.empty[V, PartId])), scheme)
        case GCons(adj, _) =>
          GCons[S, V, E](adj + (v -> adj.getOrElse(v,
            (p, Map.empty[V, PartId], Map.empty[V, PartId]))), scheme)
      }

      /** The partitions themselves, index accessible */
      override def partitions[V, E[_] : Edge](g: LogicalParGraph[S, V, E]): Vector[LogicalParGraph[S, V, E]] = g.partitions.toVector

      /** Returns the partition id associated with a specific vertex */
      override def partitionOf[V, E[_] : Edge](g: LogicalParGraph[S, V, E], v: V): Option[PartId] = g.partitionOf(v)

      /** Moves a vertex from one partition to another */
      override def updatePartitionOf[V, E[_] : Edge](g: LogicalParGraph[S, V, E], v: V, idx: PartId): LogicalParGraph[S, V, E] = g.updatePartition(v, idx)

      //TODO: Stop needing a parScheme object. Just a typeclass instance should do?
      override def point[V, E[_]: Edge](e: E[V]): LogicalParGraph[S, V, E] = LogicalParGraph[S, V, E](e)  

      //TODO: Look at using Stream, Streaming or Seq to represent this - Iterator is mutable!
      override def vertices[V, E[_] : Edge](g: LogicalParGraph[S, V, E]): Iterator[V] = g.vertices

      override def edges[V, E[_] : Edge](g: LogicalParGraph[S, V, E]): Iterator[E[V]] = g.edges

      override def plusEdge[V, E[_] : Edge](g: LogicalParGraph[S, V, E], e: E[V]): LogicalParGraph[S, V, E] = {

        //TODO: Reason about the below in the presence of self-loops when you have a moment, suspect broken
        //TODO: Work out how below is supposed to play nice with LDG?
        //Get the left hand vertex of Edge e
        val l = Edge[E].left(e)
        //Get the right hand vertex of Edge e
        val r = Edge[E].right(e)
        //Get the neighbourhood of l from g if it exists
        val lN = g.adj.get(l)
        //Get the neighbourhood of r from g if it exists
        val rN = g.adj.get(r)
        //Get the partitions for vertices l & r

        //If a vertex arrives, it is partitioned normally.
        //This code should really know as little as possible
        //So ... when an edge arrves, we call partitionEdge, regardless of whether of not the vertices already exist?
//        val (dScheme, lPart) = lN.fold(ParScheme[S].partitionForVertex(g.scheme, l, g))(n => (g.scheme, n._1))
//        val (ddScheme, rPart) = rN.fold(ParScheme[S].partitionForVertex(dScheme, r, g))(n => (dScheme, n._1))

        val (dScheme, (lPart, rPart)) = ParScheme[S].partitionForEdge(g.scheme, e, g)
        //Add r to the outgoing neighbours (l->r convention) of l either in an existing or empty neighbourhood
        val dAdj = lN.fold(g.adj + (l -> (lPart, Map.empty[V, PartId], Map(r -> rPart)))) { case (part, inN, outN) =>
          g.adj + (l -> (part, inN, outN + (r -> rPart)))
        }
        //Add l to the incoming neighbours (l->r convention) of r either in an existing or empty neighbourhood
        val ddAdj = rN.fold(dAdj + (r -> (rPart, Map(l -> lPart), Map.empty[V, PartId]))) { case (part, inN, outN) =>
          dAdj + (r -> (part, inN + (l -> lPart), outN))
        }
        GCons[S, V, E](ddAdj, dScheme)
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
        ParScheme[S].partitionForVertex(g.scheme, v, g, assign[V, E] _)
      }

      override def minusVertex[V, E[_] : Edge](g: LogicalParGraph[S, V, E], v: V): LogicalParGraph[S, V, E] = g match {
        case NullGraph(s) => g
        case GCons(adj, scheme) =>
          neighbourhood(g, v) match {
              case Some(n) if g.size <= 1 => NullGraph[S, V, E](scheme)
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

