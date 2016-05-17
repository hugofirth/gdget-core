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
import org.gdget.{Edge, Graph}

import language.higherKinds

/** Simple proof of concept partitioned main-memory graph */
sealed trait PartitionedGraph[G[_, _[_]], V, E[_]] {

  /** Make sure that G has a Graph */
  implicit def G: Graph[G]

  /** The partitions themselves, index accessible */
  def partitions: Vector[G[V, E]]
}

object PartitionedGraph extends PartitionedGraphInstances

trait PartitionedGraphInstances {

  implicit def partitionedGraphLike[G[_, _[_]]: Graph]: Graph[PartitionedGraph[G, ?, ?[_]]] =
    new Graph[PartitionedGraph[G, ?, ?[_]]] {

      //TODO: Do all the below with s.c.Future - will mean that I need an ExecutionContext in PartitionedGraph?

      //TODO: Look at using Stream, Streaming or Seq to represent this - Iterator is mutable!
      override def vertices[V, E[_] : Edge](g: PartitionedGraph[G, V, E]): Iterator[V] =
        g.partitions.foldLeft(Iterator.empty[V])((vs, part) => vs ++ Graph[G].vertices(part))

      override def minusEdge[V, E[_] : Edge](g: PartitionedGraph[G, V, E], e: E[V]): PartitionedGraph[G, V, E] = ???

      override def edges[V, E[_] : Edge](g: PartitionedGraph[G, V, E]): Iterator[E[V]] = ???

      override def plusEdge[V, E[_] : Edge](g: PartitionedGraph[G, V, E], e: E[V]): PartitionedGraph[G, V, E] = ???

      override def minusVertex[V, E[_] : Edge](g: PartitionedGraph[G, V, E], v: V): PartitionedGraph[G, V, E] = ???

      override def neighbourhood[V, E[_] : Edge](g: PartitionedGraph[G, V, E], v: V): Option[UNeighbourhood[V, E]] = ???

      override def plusVertex[V, E[_] : Edge](g: PartitionedGraph[G, V, E], v: V): PartitionedGraph[G, V, E] = ???
    }
}



