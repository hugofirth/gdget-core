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
trait Edge[E[+_]] extends LabelledEdge[({ type λ[a, _ <: Unit] = E[a]})#λ, Unit] {

  override def label[V](e: E[V]) = ()

  override def connect[V](left: V, right: V, label: Unit = ()): E[V] 
}

object Edge {

  @inline def apply[E[_]: Edge]: Edge[E] = implicitly[Edge[E]]

  implicit class EdgeOps[E[_]: Edge, V](self: E[V]) {
    def vertices = Edge[E].vertices(self)
    def left = Edge[E].left(self)
    def right = Edge[E].right(self)
    def other(v: V) = Edge[E].other(self, v)
    def connect(left: V, right: V) = Edge[E].connect(left, right)
  }

}



