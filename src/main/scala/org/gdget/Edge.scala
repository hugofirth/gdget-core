/** gdget-core
  *
  * Copyright (c) 2015 Hugo Firth
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

import language.{higherKinds, reflectiveCalls}

/** The base TypeClass for defining behaviour for all edges stored in a graph.
  *
  * Types which implement EdgeLike are considered to be unbaised, undirected edges.
  *
  * @tparam E The type implementing edge like functionality
  * @author hugofirth
  * @since 0.1
  */
trait Edge[E[_]] extends LabelledEdge[({ type λ[a, b <: Unit, c <: V0] = E[a]})#λ]  {
  //TODO: Find way to fix below
  override def connect[L <: V, R <: V, V, Lbl](left: L, right: R, label: Lbl = ()): E[L]
}

object Edge {

  //TODO: Look into inline apply[E] = implicitly[Edge[E]] to make boilerplate better.

  @inline def apply[E[_]: Edge]: Edge[E] = implicitly[Edge[E]]


  implicit class EdgeOps[E[_], V](self: E[V])(implicit val ev: Edge[E]) {
    def vertices = ev.vertices(self)
    def left = ev.left(self)
    def right = ev.right(self)
    def other(v: V) = ev.other(self, v)
    def connect(left: V, right: V) = ev.connect(left, right)
  }

}



