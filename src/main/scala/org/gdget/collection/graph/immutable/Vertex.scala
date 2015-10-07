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



import language.higherKinds

/** Instances of this trait implement the [[org.gdget.collection.graph.VertexLike]] type class.
  *
  * @see [[org.gdget.collection.graph.VertexLike]]
  * @tparam A The type of value to be represented by the vertex in a graph.
  * @author hugofirth
  * @since 0.1
  */
sealed trait Vertex[+A] {
  type E <: Edge[_, _, _]
  def value: A
  def edges: Set[E]
}

/** The OuterVertex case class represents vertices which are not explicitly members of a graph.
  *
  * Obviously any vertex with incident edges is effectively a graph, which is defined as a set of vertices and edges
  * `G = (V,E)`. However, for the purposes of the Gdget API, it is useful to have vertex representations which are not
  * associated with an instance of a [[org.gdget.collection.graph.GraphLike]] collection.
  *
  * This is the most basic vertex type in [[org.gdget.collection.graph]] and also the default implementation returned
  * from the `Vertex(...)` factory in [[org.gdget.collection.graph.Vertex]].
  *
  * @see [[Edge]]
  * @param value The value to be represented by the vertex
  * @param edges The set of [[Edge]]s connecting this vertex
  * @tparam A The type of value to be represented by the vertex
  * @author hugofirth
  * @since 0.1
  */
case class OuterVertex[A](value: A, edges: Set[OuterEdge[_, _, _]])  extends Vertex[A] { type E = OuterEdge[_, _, _] }

/** The InnerVertex case class represents vertices which are members of a graph.
  *
  * @see [[Graph]]
  * @see [[Edge]]
  * @param value The value to be represented by the vertex
  * @param edges The set of [[Edge]]s connecting this vertex
  * @param graph The [[Graph]] which this vertex is a member of
  * @tparam A The type of value to be represented by the vertex in a graph.
  * @author hugofirth
  * @since 0.1
  */
case class InnerVertex[A, G](value: A, edges: Set[InnerEdge[_, _, _]], graph: G)
                            (implicit ev: GraphLike[G, InnerVertex, InnerEdge])
  extends Vertex[A] { type E = InnerEdge[_, _, _] }


object Vertex {


  implicit def vertexLikeOuterVertex[A]: ExternalVertexLike[OuterVertex, A, OuterEdge] =
    new ExternalVertexLike[OuterVertex, A, OuterEdge] {

    override type Inner[G] = InnerVertex[A, G]

    override implicit def edgeLikeE: ExternalEdgeLike[OuterEdge, OuterVertex, _, _, _] = Edge.edgeLikeOuterEdge[_, _, _]

    override def toInner[G](elem: OuterVertex[A], graph: G)
                           (implicit ev: InternalElement[Inner[G], G], gEv: GraphLike[G, _, _]): Inner[G] =
      InnerVertex(elem.value, Set.empty[InnerEdge[_, _, _]], graph)

    override def get(v: OuterVertex[A]): A = v.value

    override def edges(v: OuterVertex[A]): Set[OuterEdge[_, _, _]] = v.edges

    override def plusEdge(v: OuterVertex[A], e: OuterEdge[_, _, _]): OuterVertex[A] = v.copy(edges = v.edges + e)

    override def minusEdge(v: OuterVertex[A], e: OuterEdge[_, _, _]): OuterVertex[A] = v.copy(edges = v.edges - e)

    override def neighbours[N >: OuterVertex[A]](v: OuterVertex[A]): Set[N] = v.edges flatMap(_.other(v))
  }

  implicit def vertexLikeInnerVertex[A, G]: InternalVertexLike[InnerVertex, A, InnerEdge, G] =
    new InternalVertexLike[InnerVertex, A, InnerEdge, G] {

    override type Outer = OuterVertex[A]

    override implicit def edgeLikeE: InternalEdgeLike[InnerEdge, InnerVertex, _, _, _, G] =
      Edge.edgeLikeInnerEdge[_, _, _, G]

    override def graph(elem: InnerVertex[A, G])(implicit gEv: GraphLike[G, _, _]): G = elem.graph

    override def toOuter(elem: InnerVertex[A, G])(implicit ev: ExternalElement[Outer]): Outer =
      OuterVertex(elem.value, Set.empty[OuterEdge[_, _, _]])

    override def get(v: InnerVertex[A, G]): A = v.value

    override def edges(v: InnerVertex[A, G]): Set[InnerEdge[_, _, _]] = v.edges

    override def minusEdge(v: InnerVertex[A, G], e: InnerEdge[_, _, _]): InnerVertex[A, G] = v.copy(edges = v.edges - e)

    override def plusEdge(v: InnerVertex[A, G], e: InnerEdge[_, _, _]): InnerVertex[A, G] = v.copy(edges = v.edges + e)

    override def neighbours[N >: InnerVertex[A, G]](v: InnerVertex[A, G]): Set[N] = v.edges flatMap(_.other(v))
  }
}


