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

import language.higherKinds

/** Simple typeclass for vertex => PartitionId mappings */
trait PartitionScheme[S[_]]{

  import PartitionScheme._

  def getPartition[V](scheme: S[V], vertex: V): PartitionId
}

object PartitionScheme {

  @inline def apply[S[_]: PartitionScheme]: PartitionScheme[S] = implicitly[PartitionScheme[S]]

  /** Wrapper type for partition ids */
  case class PartitionId(id: Int)

  /** Default instance for Map[V, Int] */
  implicit val mapScheme: PartitionScheme[Map[?, PartitionId]] = new PartitionScheme[Map[?, PartitionId]] {

    /** Default partition index if a  */
    private val defaultPartition = PartitionId(0)

    override def getPartition[V](scheme: Map[V, PartitionId], vertex: V): PartitionId = scheme.getOrElse(vertex, defaultPartition)
  }

  /** Default instance for (V) => Int */
  implicit val fun1Scheme: PartitionScheme[? => PartitionId] = new PartitionScheme[? => PartitionId] {
    override def getPartition[V](scheme: (V) => PartitionId, vertex: V): PartitionId = scheme(vertex)
  }
}
