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
package org.gdget.collection.graph.immutable

import org.gdget.collection.graph._
import org.gdget.collection.graph.EdgeLike._

/** Description of Class
  *
  * @author hugofirth
  */
object Implicits {

  implicit def edgeLikeOuterEdge[L, A, R]: EdgeLike[OuterEdge, OuterVertex, L, A, R] =
    new EdgeLike[OuterEdge, OuterVertex, L, A, R] {

      override def get(e: OuterEdge[L, A, R]): A = e.value

      override def vertices(e: OuterEdge[L, A, R])
                           (implicit vEv: VertexLike[OuterVertex, _, OuterEdge]): (OuterVertex[L], OuterVertex[R]) =
        (e.left, e.right)

      override def left(e: OuterEdge[L, A, R])
                       (implicit vEv: VertexLike[OuterVertex, _, OuterEdge]): OuterVertex[L] = e.left

      override def right(e: OuterEdge[L, A, R])
                        (implicit vEv: VertexLike[OuterVertex, _, OuterEdge]): OuterVertex[R] = e.right

      override def other(e: OuterEdge[L, A, R], v: OuterVertex[_])
                        (implicit vEv: VertexLike[OuterVertex, _, OuterEdge]): Option[OuterVertex[_]] = v match {
        case e.left => Some(e.right)
        case e.right => Some(e.left)
        case _ => None
      }
    }

  implicit def graphLikeImmutableGraph: GraphLike[Graph, InnerVertex, InnerEdge] =
    new GraphLike[Graph, InnerVertex, InnerEdge] {

      override implicit val internalVertex = vertexLikeImmutableInnerVertex

      override implicit val internalEdge = edgeLikeImmutableInnerEdge

      override def vertices(g: Graph): Iterable[InnerVertex[_]] = g.vertices

      override def edges(g: Graph): Iterable[InnerEdge[_, _, _]] = g.edges

      override def findVertex(g: Graph)(f: InnerVertex[_] => Boolean): Option[InnerVertex[_]] = g.vertices find f

      override def findEdge(g: Graph)(f: InnerEdge[_, _, _] => Boolean): Option[InnerEdge[_, _, _]] = g.edges find f

      override def minusEdge(g: Graph, e: InnerEdge[_, _, _]): Graph = g.copy(edges = g.edges - e)

      override def plusEdge(g: Graph, e: InnerEdge[_, _, _]): Graph = g.copy(edges = g.edges + e)

      override def minusVertex(g: Graph, v: InnerVertex[_]): Graph = g.copy(vertices = g.vertices - v)

      override def plusVertex(g: Graph, v: InnerVertex[_]): Graph = g.copy(vertices = g.vertices + v)
    }
}
