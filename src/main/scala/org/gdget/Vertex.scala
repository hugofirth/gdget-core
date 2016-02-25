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

import scala.language.{existentials, higherKinds}

/** The base TypeClass for defining behaviour for a vertex
  *
  * @see [[Edge]]
  * @tparam V The type implementing vertex like functionality
  * @author hugofirth
  * @since 0.1
  */
trait Vertex[V] { self =>
  /** Each Vertex instance must define an E type member which has an instance of Edge */
  type E
  implicit def edgeE: Edge[E]

  def edges(v: V): Set[E]
  def plusEdge(v: V, e: E): V
  def minusEdge(v: V, e: E): V
  def plusEdges(v: V, e: E*): V
  def minusEdges(v: V, e: E*): V
  def neighbours(v: V): Set[V]
  def degree(v: V): Int = self.edges(v).size
}

object Vertex {

  type Aux[V0, E0] = Vertex[V0] { type E = E0 }

  implicit class VertexOps[V: Vertex](v: V) {
    val vEv = implicitly[Vertex[V]]
    def edges = vEv.edges(v)
    def plusEdge(e: vEv.E) = vEv.plusEdge(v, e)
    def minusEdge(e: vEv.E) = vEv.minusEdge(v, e)
    def plusEdges(e: vEv.E*) = vEv.plusEdges(v, e: _*)
    def minusEdge(e: vEv.E*) = vEv.minusEdges(v, e: _*)
    def neighbours = vEv.neighbours(v)
    def degree = vEv.degree(v)
  }
}

