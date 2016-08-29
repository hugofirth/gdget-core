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
package org.gdget.partitioned

import cats.data.State
import language.higherKinds

import org.gdget.{Graph, Edge}

/** Simple typeclass for vertex => PartitionId mappings */
trait ParScheme[S[_]]{

//  import ParScheme._
//
//  //Some kind of default method here. May take no parameters, or a vertex, or an edge, or possibly just a type param.
//  // Try and find other parameterless methods on typeclasses in Cats and Scalaz.
//
//  //TODO: Partitioner API overhaul. Do not have ParGraphs account for a ParScheme at all. Is overcomplicating things.
//  def default[V]: S[V]
//
//  //TODO: Use State here - but its a pain in the ass
//  //What if, instead, a partition method took a function (V, G[V, E], PartId) => G[V, E]? Could then
//  def partitionForVertex[G[_, _[_]]: ParGraph, E[_]: Edge, V](scheme: S[V],
//                                                              vertex: V,
//                                                              graph: G[V, E],
//                                                              assignV: (V, G[V, E], PartId, S[V]) => G[V, E]): G[V, E]
//
//  def partitionForEdge[G[_, _[_]]: ParGraph, E[_]: Edge, V](scheme: S[V],
//                                                            edge: E[V],
//                                                            graph: G[V, E],
//                                                            assignV: (V, G[V, E], PartId, S[V]) => G[V, E]): G[V, E]
}

object ParScheme {

//  @inline def apply[S[_]: ParScheme]: ParScheme[S] = implicitly[ParScheme[S]]
//
//
//
//
//  /** default partition index if a vertex does not belong to the domain of the getPartition function */
//  private val defaultPartition = 0.part
//
//  /** Note: Simple ParScheme instances below don't carry state, but that does not mean that ParSchemes cannot do so! */
//
//  /** default instance for Map[V, Int] */
//  implicit val mapInstance: ParScheme[Map[?, PartId]] = new ParScheme[Map[?, PartId]] {
//
//    override def default[V] = Map.empty[V, PartId]
//
//    override def partitionForVertex[G[_, _[_]]: ParGraph, E[_]: Edge, V](scheme: Map[V, PartId],
//                                                                         vertex: V,
//                                                                         graph: G[V, E],
//                                                                         assignV: (E, G[V, E], PartId, Map[V, PartId]) => G[V, E]): G[V, E] =
//      assignV(vertex, graph, scheme.getOrElse(vertex, defaultPartition), scheme)
//
//    override def partitionForEdge[G[_,_[_]]: ParGraph, E[_]: Edge, V](scheme: Map[V, PartId],
//                                                                      edge: E[V],
//                                                                      graph: G[V, E],
//                                                                      assignV: (V, G[V, E], PartId, Map[V, PartId]) => G[V, E]): G[V, E] = {
//      val l = Edge[E].left(edge)
//      val r = Edge[E].right(edge)
//      val f = (v: V) => assignV(v, _: G[V, E], scheme.getOrElse(v, defaultPartition), scheme)
//      (f(l) compose f(l))(graph)
//
//    }
//
//  }
//
//  /** default instance for (V) => Int */
//  implicit val fun1sScheme: ParScheme[? => PartId] = new ParScheme[? => PartId] {
//
//    override def default[V] = (v: V) => defaultPartition
//
//    override def partitionForVertex[G[_, _[_]]: ParGraph, E[_]: Edge, V](scheme: (V) => PartId,
//                                                                         vertex: V,
//                                                                         graph: G[V, E],
//                                                                         assignV: (V, G[V, E], PartId, (V) => PartId) => G[V, E]): G[V, E] =
//      assignV(vertex, graph, scheme(vertex), scheme)
//
//    override def partitionForEdge[G[_, _[_]] : ParGraph, E[_] : Edge, V](scheme: (V) => PartId,
//                                                                         edge: E[V],
//                                                                         graph: G[V, E],
//                                                                         assignV: (V, G[V, E], PartId, (V) => PartId) => G[V, E]): G[V, E] = {
//
//      //TODO: Have a careful think about this. Out experience with LDG suggests that just blindly partitioning both vertices
//      //As if they arrived separately without context may not be a good idea.
//      val l = Edge[E].left(edge)
//      val r = Edge[E].right(edge)
//      val f = (v: V) => assignV(v, _: G[V, E], scheme(v), scheme)
//      (f(l) compose f(r))(graph)
//    }
//
//  }
}
