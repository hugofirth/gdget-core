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

import scala.annotation.implicitNotFound
import scala.language.{higherKinds, reflectiveCalls}

/** Description of Class
  *
  * @author hugofirth
  */
@implicitNotFound("No member of type class LEdge found for types ${E} and ${L}")
trait LEdge[E[_, _], L] extends Any with Serializable  {

  def label[V](e: E[V, L]): L

  def connect[V](left: V, right: V, label: L): E[V, L]

  def vertices[V](e: E[V, L]): (V, V)

  def left[V](e: E[V, L]): V

  def right[V](e: E[V, L]): V

  def other[V](e: E[V, L], v: V): Option[V]
}


object LEdge {

  @inline def apply[E[_, _], L](implicit ev: LEdge[E, L]): LEdge[E, L] = ev

  implicit class LEdgeOps[E[_, _], V, L](e: E[V, L])(implicit val ev: LEdge[E, L]) {
    //TODO: Work out inheritance heirarchy for LEdgeOps and EdgeOps which means there are no ambiguous implicits
    def label = LEdge[E, L].label(e)
    def vertices = LEdge[E, L].vertices(e)
    def left = LEdge[E, L].left(e)
    def right = LEdge[E, L].right(e)
    def other(v: V) = LEdge[E, L].other(e, v)
  }
}