/** gdget-core
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
package org.gdget.labelled

import language.higherKinds
import scala.reflect.runtime.universe._

import org.gdget.Edge

/** Description of Class
  *
  * @author hugofirth
  */
trait LEdge[E[_ <: V0, _ <: V0], L <: V0, R <: V0, V0] extends Edge[E[L, R]] {

  override type V = V0

  def label = typeTag[E[L,R]]

  override def vertices(e: E[L, R]): (L, R)

  override def left(e: E[L, R]): L

  override def right(e: E[L, R]): R
}


object LEdge {

  implicit class LEdgeOps[E[_ <: V, _ <: V], L <: V, R <: V, V](e: E[L, R])(implicit override val ev: LEdge[E, L, R, V]) extends
    Edge.EdgeOps[E[L, R]](e) {

    def label = ev.label
    override def vertices: (L, R) = ev.vertices(e)
    override def left: L = ev.left(e)
    override def right: R = ev.right(e)

  }
}