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

import cats.free.Free
import language.higherKinds

/** Description of Class
  *
  * @author hugofirth
  */
package object data {

  /** [[cats.free.Free]] type for Graph Queries based upon QueryOp ADT */
  type QueryIO[G[_, _[_]], V, E[_], A] = Free[QueryOp[G, V, E, ?], A]

  import cats.{Id, ~>}

  def interpreter[G[_, _[_]], V, E[_]](implicit gEv: Graph[G], eEv: Edge[E]) = new (QueryOp[G, V, E, ?] ~> Id) {

    def apply[A](fa: QueryOp[G, V, E, A]): Id[A] =
      fa match {
        case Get(v, g) => {
          println(s"Get $v")
          Graph[G].getVertex(g, v)
        }
        case GetWhere(condition, g) => Graph[G].vertices(g).filter(condition).toList
        case TraverseEdge(v, e, g) => {
          println(s"Traverse edge $e of $v")
          val n = Graph[G].neighbourhood(g, v)
          n.fold(None: Option[E[V]])(_.edges.find(_ == e))
        }
        case TraverseInNeighbour(v, inV, g) => {
          println(s"Traverse incoming neighbour of $v")
          val n = Graph[G].neighbourhood(g, v)
          n.fold(None: Option[V])(_.in.keySet.find(_ == inV))
        }
        case TraverseOutNeighbour(v, outV, g) => {
          println(s"Traverse outgoing neighbour of $v")
          val n = Graph[G].neighbourhood(g, v)
          n.fold(None: Option[V])(_.out.keySet.find(_ == outV))
        }
      }
  }

}
