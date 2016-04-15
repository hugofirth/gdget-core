///** gdget-core
//  *
//  * Copyright (c) 2016 Hugo Firth
//  * Email: <me@hugofirth.com/>
//  *
//  * Licensed under the Apache License, Version 2.0 (the "License");
//  * you may not use this file except in compliance with the License.
//  * You may obtain a copy of the License at:
//  *
//  * http://www.apache.org/licenses/LICENSE-2.0
//  *
//  * Unless required by applicable law or agreed to in writing, software
//  * distributed under the License is distributed on an "AS IS" BASIS,
//  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  * See the License for the specific language governing permissions and
//  * limitations under the License.
//  */
//package org.gdget.collection
//
//import cats._
//import cats.std.all._
//import cats.syntax.eq._
//import org.gdget._
//
//import scala.collection.BitSet
//import language.higherKinds
//
///** LabelledGraph is a directed, labelled graph. It is designed to be resident in memory and relatively space efficient,
//  * storing upto 200M edges without needing to spill to disk. It provides instances of the [[algebra.Monoid]] &
//  * [[org.gdget.Graph]] typeclasses.
//  *
//  * LabelledGraph is intended to be the goto graph implementation in gdget.
//  *
//  * **Note** that if our TypeTag/isInstanceOf tomfoolery does not work we can use a byte and a lookup table to do labels.
//  *
//  * TODO: update naive implementation to avoid having to store full edges. Can we cast from a typeTag? IF so can we
//  * store neighbours like so: (Label, Map[V, Set[Label] ], Map[V, Set[Label] ]) then have a metadata map in the Graph
//  * instance Map[Label, TypeTag] and then for each set lookup the typetag for each entry in each map, and create the
//  * Edges of the right types (with a cast) only when we need to?
//  *
//  * Other option is to have E type parameter of LabelledGraph be an ADT of Labels and then have a single edge type
//  * (LabelledEdge) for which takes a type parameter and a parameter of type L (the label) where L is subtype of E. This
//  * would still grant the compile time safety, and would make our lives simpler.
//  *
//  * @see [[LabelledNeighbourhood]]
//  * @author hugofirth
//  */
////TODO: Make this into a trait
//sealed abstract class LabelledGraph[V, E] {
//
//  import LabelledGraph._
//
//  private[gdget] def adj: AdjacencyList[V]
//
//  implicit def E: LEdge[E]
//
//  //TODO: Implement as val and make private?
//  def labels: Map[Int, E]
//
//  def size: Long
//  def order: Long
//
//  def vertices = adj.keys.iterator
//
//  def edges = for {
//    (v, (inEdges, outEdges)) <- adj.toIterator
//    (neighbour, edgeLabels) <- inEdges
//    label <- edgeLabels
//  } yield LabelledEdge(neighbour, labels(label), v)
//
//}
//
//object LabelledGraph {
//
//  type AdjacencyList[V] = Map[V, (Map[V, BitSet], Map[V, BitSet])]
//
//  def empty[V, E]: LabelledGraph[V, E] = NullGraph[V, E]
//
//  final def apply[V, E: Labellable](es: LabelledEdge[V, E, V]*): LabelledGraph[V, E] = ???
//
//  private[gdget] final case class GCons[V, E](adj: AdjacencyList[V]) extends LabelledGraph[V, E] {
//    override lazy val size: Long = vertices.size
//    override lazy val order: Long = edges.size
//  }
//
//  private[gdget] case object NullGraph extends LabelledGraph[Nothing, Nothing] {
//    val size = 0L
//    val order = 0L
//
//    override private[gdget] val adj: AdjacencyList[Nothing] =
//      Map.empty[Nothing, (Map[Nothing, BitSet], Map[Nothing, BitSet])]
//
//    //TODO: Find out what this unapply is doing (lifted verbatim from Scalaz Map code)
//    def unapply[V, E](g: LabelledGraph[V, E]): Boolean = g eq this
//
//    def apply[V, E]: LabelledGraph[V, E] = this.asInstanceOf[LabelledGraph[V, E]]
//  }
//}
//
///**
//  *
//  * @param center the vertex upon which this neighbourhood is centered
//  * @param in
//  * @param out
//  * @tparam V
//  */
//final case class LabelledNeighbourhood[V](center: V, in: Map[V, BitSet], out: Map[V, BitSet])
//
//
//final case class LabelledEdge[+L, E: Labellable, +R](source: L, label: E, destination: R)
//
//trait LabelledGraphInstances {
//  import LabelledGraph._
//
//  implicit def labelledGraphMonoid[V, E](implicit ev: Monoid[AdjacencyList[V]]) = new Monoid[LabelledGraph[V, E]] {
//    def empty = LabelledGraph.empty[V, E]
//
//    def combine(x: LabelledGraph[V, E], y: LabelledGraph[V, E]) =
//      GCons(Monoid[AdjacencyList[V]].combine(x.adj, y.adj))
//  }
//
//}
//
//private[gdget] sealed trait LabelledGraphLike[V, E] extends Graph[LabelledGraph, V, LabelledEdge[V, E, V]] {
//  import LabelledGraph._
//
//  type N[V0, E0] = LabelledNeighbourhood[V0]
//
//  override def size(g: LabelledGraph[V, LabelledEdge[V, E, V]]) = g.size
//
//  override def order(g:LabelledGraph[V, LabelledEdge[V, E, V]]) = g.order
//
//  override def vertices(g: LabelledGraph[V, LabelledEdge[V, E, V]]) = g.vertices
//
//  //TODO: Fix below vvv
//  override def edges(g: LabelledGraph[V, LabelledEdge[V, E, V]]) = g.edges
//
//
//
//}
//
//private[gdget] sealed trait LabelledNeighbourhoodLike[V, E] extends Neighbourhood[LabelledGraph, V, E] {}
//
//private[gdget] sealed trait LabelledEdgeLike[L <: V, R <: V, V, E] extends LEdge[LabelledEdge, L , R, V, E] {
//
//  override def vertices(e: LabelledEdge[L, E, R]) = (e.source, e.destination)
//
//  override def label(e: LabelledEdge[L, E, R]) = e.label
//
//  override def left(e: LabelledEdge[L, E, R]): L = e.source
//
//  override def right(e: LabelledEdge[L, E, R]): R = e.destination
//
//  override def connect(left: L, label: E, right: R): LabelledEdge[L, E, R] = LabelledEdge(left, label, right)
//
//
//  //TODO: Use Cats.Eq
//  override def other(e: LabelledEdge[L, E, R], v: V) =
//    if(e.source == v)
//      Option(e.destination)
//    else if(e.destination == v)
//      Option(e.source)
//    else
//      None
//
//}
//
