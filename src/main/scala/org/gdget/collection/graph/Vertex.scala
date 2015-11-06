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
package org.gdget.collection.graph

import language.{higherKinds, existentials}

import scalaz._
import Scalaz._

/** The base TypeClass for defining behaviour for a vertex
  *
  * @see [[Edge]]
  * @tparam V The type implementing vertex like functionality
  * @author hugofirth
  * @since 0.1
  */
trait Vertex[V] extends Element[V] { self =>

  def edges(v: V): Set[(E[_, _], Edge[E]) forSome { type E[_,_] }]
  def plusEdge[E[_, _]](v: V, e: E[_, _])(implicit ev: Edge[E]): V
  def minusEdge[E[_, _]](v: V, e: E[_, _])(implicit ev: Edge[E]): V
  def neighbours(v: V): Set[(V, Vertex[V]) forSome { type V }]
  def degree(v: V): Int = self.edges(v).size
  def isContainedIn(v: V, other: V): Boolean = (v === other) && self.edges(v).subsetOf(self.edges(other))

}

object Vertex {

  implicit class VertexOps[V](v: V)(implicit vEv: Vertex[V]) {
    def edges = vEv.edges(v)
    def plusEdge[E[_, _]](e: E[_, _])(implicit ev: Edge[E]) = vEv.plusEdge(v, e)
    def minusEdge[E[_, _]](e: E[_, _])(implicit ev: Edge[E]) = vEv.minusEdge(v, e)
    def neighbours = vEv.neighbours(v)
    def degree = vEv.degree(v)
    def isContainedIn(other: V) = vEv.isContainedIn(v, other)
  }
}

