/** gdget
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

import org.gdget.{Edge, LEdge}

import language.higherKinds

/** Description of Class
  *
  * TODO: Neighbourhood is going to get used everywhere, so we should put the effort in to specialise
  *
  * @author hugofirth
  */
trait Neighbourhood[V, E[_, _], L] {

  implicit def E: LEdge[E, L]

  def center: V
  def in: Map[V, Set[L]]
  def out: Map[V, Set[L]]

  def neighbours = in.keySet ++ out.keySet

  def neighboursIterator = in.keysIterator ++ out.keysIterator

  //TODO: Fix below, by getting rid of the toSet calls.

  def inEdges = in.flatMap { case (v, l) => l.map(LEdge[E, L].connect(v, center, _)) }.toSet

  def outEdges = out.flatMap { case (v, l) => l.map(LEdge[E, L].connect(center, v, _)) }.toSet

  def edges = inEdges ++ outEdges

}


final case class LNeighbourhood[V, E[_, _], L](center: V, in: Map[V, Set[L]], out: Map[V, Set[L]])
                                              (implicit val E: LEdge[E, L]) extends Neighbourhood[V, E, L]

//TODO: Double check rush job with this class


final case class UNeighbourhood[V, E[_]](center: V, in: Map[V, Set[Unit]], out: Map[V, Set[Unit]])(implicit val E: Edge[E])
  extends Neighbourhood[V, Lambda[(A, B) => E[A]], Unit]
