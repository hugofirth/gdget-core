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

import language.higherKinds

/** The Neighbourhood typeclass represents the closed neighbourhood of a given center vertex v; i.e. the induced
  * sub-graph of a graph G which includes v and all v's adjacent vertices.
  *
  * @tparam N The type implementing graph like functionality
  * @tparam V The vertex type contained in a graph of type N
  * @tparam E the edge type connecting vertices of type V, in a graph of type N
  * @see [[Graph]]
  * @author hugofirth
  */
trait Neighbourhood[N[_, _], V, E] extends Graph[N, V, E] { self =>

  def degree(n: N[V, E]): Long = self.size(n)

  def center(n: N[V, E]): V

  def neighbours(n: N[V, E]): Iterator[V] = self.vertices(n)

  def edges(n: N[V, E]): Iterator[E] = self.edges(n)
}
