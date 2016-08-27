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
import scala.annotation.implicitNotFound


/** Simple typeclass for vertex partitioned graphs */
@implicitNotFound("No member of type class ParGraph found for type ${G}")
trait ParGraph[G[_, _[_]], V, E[_]] extends Graph[G, V, E[_]]{

  /** Ensure that the type V has a ParVertex typeclass instance */
  def V: ParVertex[V]

  /** The number of partitions in the ParGraph
    *
    * Note: Whilst this could be more naturally exposed as partitions.size, in some implementations (e.g. LogicalParGraph)
    * this would lead to an O(|V|) operation, which is daft. I'm not even going to use partitions.size as a default impl
    * in order to avoid te Gotcha.
    */
  def numPartitions(g: G[V, E]): Int

  /** The partitions of a graph themselves, index accessible */
  def partitions(g: G[V, E]): Vector[G[V, E]]

  /** Returns the partition id associated with a specific vertex */
  def partitionOf(g: G[V, E], v: V): Option[PartId]

  /** Moves a vertex from one partition to another */
  def updatePartitionOf(g: G[V, E], v: V, idx: PartId): G[V, E]
}

object ParGraph {
  @inline def apply[G[_, _[_]]: ParGraph, V: ParVertex, E[_]: Edge]: ParGraph[G, V, E] = implicitly[ParGraph[G, V, E]]
}




