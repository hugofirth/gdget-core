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

import org.gdget.{Edge, Graph}

import language.higherKinds

/** Simple typeclass for Partitioner objects which may split input graphs into k pieces according to some strategy. */
trait Partitioner[P[_]] extends Any with Serializable {

  /** The Scheme type for this partitioner*/
  type S[_]

  implicit def S: PartitionScheme[S]

  def partition[G[_, _[_]]: Graph, V, E[_]: Edge](strategy: P[G[V, E]], g: G[V, E], k: Int): S[V]
}
