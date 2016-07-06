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
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._


/** Simple proof of concept of a thread-partitioned main-memory graph */
//TODO: Cleanup below once I work out what its doing ?
sealed trait ParGraph[G[_, _[_]], V, E[_]] {

  /** Make sure that G has a Graph */
  implicit def G: Graph[G]

  /** The partitions themselves, index accessible */
  def partitions: Vector[G[V, E]]
}

object ParGraph extends PartitionedGraphInstances

trait PartitionedGraphInstances {

  implicit def partitionedGraphLike[G[_, _[_]]: Graph]: Graph[ParGraph[G, ?, ?[_]]] =
    new Graph[ParGraph[G, ?, ?[_]]] {

      import ExecutionContext.Implicits.global

      //TODO: How best to do this? Simple lazy op like below cheaper sync, but in general Futures or Actors?

      override def vertices[V, E[_] : Edge](g: ParGraph[G, V, E]): Iterator[V] =
       Await.result(Future.reduce(g.partitions.map(p => Future { Graph[G].vertices(p) }))(_ ++ _), Duration.Inf)

      override def minusEdge[V, E[_] : Edge](g: ParGraph[G, V, E], e: E[V]): ParGraph[G, V, E] = ???

      override def edges[V, E[_] : Edge](g: ParGraph[G, V, E]): Iterator[E[V]] = ???

      override def plusEdge[V, E[_] : Edge](g: ParGraph[G, V, E], e: E[V]): ParGraph[G, V, E] = ???

      override def minusVertex[V, E[_] : Edge](g: ParGraph[G, V, E], v: V): ParGraph[G, V, E] = ???

      override def neighbourhood[V, E[_] : Edge](g: ParGraph[G, V, E], v: V): Option[UNeighbourhood[V, E]] = ???

      override def plusVertex[V, E[_] : Edge](g: ParGraph[G, V, E], v: V): ParGraph[G, V, E] = ???
    }
}



