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

import cats.data.State
import language.higherKinds

import org.gdget.{Graph, Edge}

/** Simple typeclass for vertex => PartitionId mappings */
trait ParScheme[S[_]]{

  import ParScheme._

  //TODO: Use State here - but its a pain in the ass
  def getPartition[G[_, _[_]]: Graph, E[_]: Edge, V](scheme: S[V], vertex: V, graph: G[V, E]): (S[V], PartitionId)
}

object ParScheme {

  @inline def apply[S[_]: ParScheme]: ParScheme[S] = implicitly[ParScheme[S]]

  /** Wrapper type for partition ids */
  case class PartitionId(id: Int)

  /** Default instance for Map[V, Int] */
  implicit val mapScheme: ParScheme[Map[?, PartitionId]] = new ParScheme[Map[?, PartitionId]] {

    /** Default partition index if a  */
    private val defaultPartition = PartitionId(0)

    override def getPartition[G[_, _[_]]: Graph, E[_]: Edge, V](scheme: Map[V, PartitionId], vertex: V, graph: G[V, E]): (Map[V, PartitionId], PartitionId) =
      (scheme, scheme.getOrElse(vertex, defaultPartition))
  }

  /** Default instance for (V) => Int */
  implicit val fun1Scheme: ParScheme[? => PartitionId] = new ParScheme[? => PartitionId] {
    override def getPartition[G[_, _[_]]: Graph, E[_]: Edge, V](scheme: (V) => PartitionId, vertex: V, graph: G[V, E]): ((V) => PartitionId, PartitionId) = (scheme, scheme(vertex))
  }
}
