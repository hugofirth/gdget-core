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
package org.gdget.collection

import cats._
import cats.std.all._
import cats.syntax.eq._
import org.gdget._

import language.{higherKinds, reflectiveCalls}

/** LabelledGraph is a directed, labelled graph. It is designed to be resident in memory and relatively space efficient,
  * storing upto 200M edges without needing to spill to disk. It provides instances of the [[algebra.Monoid]] &
  * [[org.gdget.Graph]] typeclasses.
  *
  * LabelledGraph is intended to be the goto graph implementation in gdget.
  *
  * @see [[LabelledNeighbourhood]]
  * @author hugofirth
  */
//TODO: Make this into a trait
sealed abstract class LabelledGraph[V, E[_, +_], L] {

  import LabelledGraph._

  private[gdget] def adj: AdjacencyList[V, L]

  implicit def E: LabelledEdge[E, L]

  def size: Int
  def order: Int

  def vertices = adj.keys.iterator

  def edges = for {
    (v, (inEdges, outEdges)) <- adj.toIterator
    (neighbour, edgeLabels) <- inEdges
    l <- edgeLabels
  } yield E.connect(neighbour, v, l)

}

object LabelledGraph {

  type AdjacencyList[V, L] = Map[V, (Map[V, Set[L]], Map[V, Set[L]])]

  def empty[V, E[_, +_], L](implicit eEv: LabelledEdge[E, L]): LabelledGraph[V, E, L] = NullGraph[V, E, L]

  final def apply[V, E[_, +_], L](es: LabelledEdge[E, L]*)(implicit ev: LabelledEdge[E, L]): LabelledGraph[V, E, L] = ???

  private[gdget] final case class GCons[V, E[_, +_], L](adj: AdjacencyList[V, L])(implicit val E: LabelledEdge[E, L]) extends LabelledGraph[V, E, L] {
    override lazy val size = vertices.size
    override lazy val order = edges.size
  }

  private[gdget] case object NullGraph extends LabelledGraph[Nothing, ({ type λ[a, +_] = (Nothing, Nothing)})#λ, Unit] {

    type EA[a, +_] = (Nothing, Nothing)

    implicit def E = LabelledEdge[EA, Unit]

    val size = 0
    val order = 0

    override private[gdget] val adj: AdjacencyList[Nothing, Nothing] =
      Map.empty[Nothing, (Map[Nothing, Set[Nothing]], Map[Nothing, Set[Nothing]])]

    def unapply[V, E[_, +_], L](g: LabelledGraph[V, E, L])(implicit ev: LabelledEdge[E, L]): Boolean = g eq this

    def apply[V, E[_, +_], L](implicit ev: LabelledEdge[E, L]): LabelledGraph[V, E, L] = this.asInstanceOf[LabelledGraph[V, E, L]]
  }
}

/**
  *
  * @param center the vertex upon which this neighbourhood is centered
  * @param in
  * @param out
  * @tparam V
  */
final case class LabelledNeighbourhood[V, L](center: V, in: Map[V, Set[L]], out: Map[V, Set[L]])

trait LabelledGraphInstances {
  import LabelledGraph._

  implicit def labelledGraphMonoid[V, E[_, +_], L](implicit mEv: Monoid[AdjacencyList[V, L]], eEv: LabelledEdge[E, L]) = new Monoid[LabelledGraph[V, E, L]] {

    def empty = LabelledGraph.empty[V, E, L]

    def combine(x: LabelledGraph[V, E, L], y: LabelledGraph[V, E, L]) =
      GCons(Monoid[AdjacencyList[V, L]].combine(x.adj, y.adj))
  }

  implicit def labelledNeighbourhood[L]: Neighbourhood[({type λ[a, _] = LabelledNeighbourhood[a, L]})#λ] =
    new Neighbourhood[({type λ[a, _] = LabelledNeighbourhood[a, L]})#λ] {

      override def neighbours[V, E[_]: Edge](n: LabelledNeighbourhood[V, L]): Iterator[V] = n.in.keysIterator ++ n.out.keysIterator

      def edges[V, E[_] : Edge](n: LabelledNeighbourhood[V, L]): Iterator[E[V]] = n.in.flatMap { case (neighbour, labels) =>
        labels.map(LabelledEdge[E].connect(_, n.center))
      } ++ n.out.flatMap { case (neighbour, labels) =>
        labels.map(LabelledEdge[E].connect(n.center, _))
      }

      override def center[V, E[_] : Edge](n: LabelledNeighbourhood[V, L]): V =  n.center
  }

}

//private[gdget] sealed trait LabelledGraphLike[V, E] extends Graph[LabelledGraph] {
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



