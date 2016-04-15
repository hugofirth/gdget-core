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
  *
  * @author hugofirth
  */
trait LabelledEdge[E[_, _], Lbl] extends Any with Serializable {

  def label[V](e: E[V, Lbl]): Lbl

  def connect[V](left: V, right: V, label: Lbl): E[V, Lbl]

  def vertices[V](e: E[V, Lbl]): (V, V)

  def left[V](e: E[V, Lbl]): V

  def right[V](e: E[V, Lbl]): V

  def other[V](e: E[V, Lbl], v: V): Option[V]
}


object LabelledEdge {

  @inline def apply[E[_, _], Lbl](implicit ev: LabelledEdge[E, Lbl]): LabelledEdge[E, Lbl] = ev

  implicit class LabelledEdgeOps[E[_, _], V, Lbl](e: E[V, Lbl])(implicit val ev: LabelledEdge[E, Lbl]) {

    def label = LabelledEdge[E, Lbl].label(e)
    def vertices = LabelledEdge[E, Lbl].vertices(e)
    def left = LabelledEdge[E, Lbl].left(e)
    def right = LabelledEdge[E, Lbl].right(e)
    def other(v: V) = LabelledEdge[E, Lbl].other(e, v)
  }
}