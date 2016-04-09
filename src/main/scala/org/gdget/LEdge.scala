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
package org.gdget

import scala.language.higherKinds
import scala.reflect.runtime.universe._

/** Description of Class
  *
  * @author hugofirth
  */
trait LEdge[E[_ <: V0, _, _ <: V0], L <: V0, R <: V0, V0, Lbl] extends Edge[E[L, Lbl, R]] {

  override type V = V0

  def label(e: E[L, Lbl, R]): Lbl

  override def vertices(e: E[L, Lbl, R]): (L, R)

  override def left(e: E[L, Lbl, R]): L

  override def right(e: E[L, Lbl, R]): R
}


object LEdge {

  implicit class LEdgeOps[E[_ <: V, _, _ <: V], L <: V, R <: V, V, Lbl](e: E[L, Lbl, R])(implicit override val ev: LEdge[E, L, R, V, Lbl]) extends
    Edge.EdgeOps[E[L, Lbl, R]](e) {

    def label = ev.label(e)
    override def vertices: (L, R) = ev.vertices(e)
    override def left: L = ev.left(e)
    override def right: R = ev.right(e)

  }
}