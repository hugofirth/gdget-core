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
  def getPartition[G[_, _[_]]: Graph, E[_]: Edge, V](scheme: S[V], vertex: V, graph: G[V, E]): (S[V], PartId)
}
      
object ParScheme {
      
  @inline def apply[S[_]: ParScheme]: ParScheme[S] = implicitly[ParScheme[S]]
      
  /** wrapper type for partition ids */
  case class PartId(id: Int)
      
  implicit class PartIdOps(id: Int) {
    def part = PartId(id)
  }

  /** default instance for Map[V, Int] */
  implicit val mapInstance: ParScheme[Map[?, PartId]] = new ParScheme[Map[?, PartId]] {
      
    /** default partition index if a vertex cannot be found in the Map */
    private val defaultPartition = 0.part
      
    override def getPartition[G[_, _[_]]: Graph, E[_]: Edge, V](scheme: Map[V, PartId],
                                                                vertex: V, graph: G[V, E]): (Map[V, PartId], PartId) =
      (scheme, scheme.getOrElse(vertex, defaultPartition))
  }
      
  /** default instance for (V) => Int */
  implicit val fun1sScheme: ParScheme[? => PartId] = new ParScheme[? => PartId] {
    override def getPartition[G[_, _[_]]: Graph, E[_]: Edge, V](scheme: (V) => PartId,
                                                                vertex: V,
                                                                graph: G[V, E]): ((V) => PartId, PartId) =
      (scheme, scheme(vertex))
  }
}
