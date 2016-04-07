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


/** The base TypeClass for defining behaviour for all edges stored in a graph.
  *
  * Types which implement EdgeLike are considered to be unbaised, undirected edges.
  *
  * @tparam E The type implementing edge like functionality
  * @author hugofirth
  * @since 0.1
  */
trait Edge[E] extends Any with Serializable {

  /** Each Edge instance must define an V type member, representing the type of vertices being connected */
  type V

  def vertices(e: E): (V, V)

  def other(e: E, v: V): Option[V]

  def left(e: E): V

  def right(e: E): V
}

object Edge {

  type Aux[E0, V0] = Edge[E0] { type V = V0 }

  //TODO: Look into inline apply[E] = implicitly[Edge[E]] to make boilerplate better.

  implicit class EdgeOps[E](e: E)(implicit val ev: Edge[E]) {
    def vertices = ev.vertices(e)
    def left = ev.left(e)
    def right = ev.right(e)
    def other(v: ev.V) = ev.other(e, v)
  }

}



