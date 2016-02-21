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
package org.gdget.instances.std

import org.gdget.util._

import language.{higherKinds, existentials}

import org.gdget.{Edge, Graph, Vertex}

/** Description of Class
  *
  * @author hugofirth
  */
trait SetInstances {

//  case class Foo(a: Int)
//
//  implicit val setGraph: Graph[Set[(V Having Vertex) forSome { type V }]] = new Graph[Set[Having[_, Vertex]]] {
//
//    type G = Set[(V Having Vertex) forSome { type V }]
//    type Vertices = Iterable[(V Having Vertex) forSome { type V }]
//    type Edges = Iterable[(E Having Edge) forSome { type E[_, _]}]
//
//    override def vertices(g: G): Vertices = g
//
//    override def edges(g: G): Edges = g flatMap {
//      case e Has ev => ev edges e
//    }
//
//    override def minusVertices[V: Vertex](g: G, v: V*): G = ???
//
//    override def plusEdges[E[_, _] : Edge, L, R](g: G, e: E[L, R]*): G = ???
//
//    override def minusEdge[E[_, _] : Edge, L, R](g: G, e: E[L, R]): G = ???
//
//    override def minusEdges[E[_, _] : Edge, L, R](g: G, e: E[L, R]*): G = ???
//
//    //Probably need to take  PartialFunction here, or delve back into CoProducts
//    //override def findVertex[V: Vertex](g: G)(f: (V) => Boolean): Option[V] = vertices(g) find { v => f(v.a) }
//
//    override def plusEdge[E[_, _] : Edge, L, R](g: Set[Having[_, Vertex]], e: E[L, R]): Set[Having[_, Vertex]] = ???
//
//    override def findEdge[E[_, _] : Edge, L, R](g: Set[Having[_, Vertex]])(f: (E[L, R]) => Boolean): Option[E[L, R]] = ???
//
//    override def minusVertex[V: Vertex](g: Set[Having[_, Vertex]], v: V): Set[Having[_, Vertex]] = ???
//
//    override def plusVertex[V: Vertex](g: Set[Having[_, Vertex]], v: V): Set[Having[_, Vertex]] = ???
//
//    override def union(lg: Set[Having[_, Vertex]], rg: Set[Having[_, Vertex]]): Set[Having[_, Vertex]] = ???
//
//    override def plusVertices[V: Vertex](g: Set[Having[_, Vertex]], v: V*): Set[Having[_, Vertex]] = ???
//
//    override def intersection(lg: Set[Having[_, Vertex]], rg: Set[Having[_, Vertex]]): Set[Having[_, Vertex]] = ???
//
//  }

}
