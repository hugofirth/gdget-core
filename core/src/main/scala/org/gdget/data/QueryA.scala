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
package org.gdget.data

import language.higherKinds
import cats.free.Free.liftF
import org.gdget.{LEdge, Path}

/** The ADT representing a basic grammar for read-only queries over collection types which provide [[org.gdget.Graph]]
  * instances.
  *
  * @author hugofirth
  */
sealed trait QueryA[A]
//TODO: Have getMatch take a Path, not a pattern graph, and have Path have a deconstructor (unapply), so that I can
//  get at individual elements of the path inside the for comprehension of the query.
case class Get[V](vertex: V) extends QueryA[List[V]]

case class WithEdge[V, E[_, _], L](vertices: List[V], edge: E[V, L])(implicit ev: LEdge[E, L])
  extends QueryA[List[Vector[E[V, L]]]]

case class WithInNeighbour[V](vertices: List[V], in: V) extends QueryA[List[Vector[V]]]

case class WithOutNeighbour[V](vertices: List[V], out: V) extends QueryA[List[Vector[V]]]

object Query {

  def get[V](vertex: V): Query[List[V]] = liftF[QueryA, List[V]](Get(vertex))

  def withEdge[V, E[_, _], L](vertices: List[V], edge: E[V, L])(implicit ev: LEdge[E, L]): Query[List[Vector[E[V, L]]]] =
    liftF[QueryA, List[Vector[E[V, L]]]](WithEdge(vertices, edge))

  def withInNeighbour[V](vertices: List[V], in: V): Query[List[Vector[V]]] =
    liftF[QueryA, List[Vector[V]]](WithInNeighbour(vertices, in))

  def withOutNeighbour[V](vertices: List[V], out: V): Query[List[Vector[V]]] =
    liftF[QueryA, List[Vector[V]]](WithInNeighbour(vertices, out))
}