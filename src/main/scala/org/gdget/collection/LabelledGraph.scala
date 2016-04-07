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

import cats.Monoid
import org.gdget.{Edge, Graph, Neighbourhood}

import scala.collection.BitSet

/** LabelledGraph is a directed, labelled graph. It is designed to be resident in memory and relatively space efficient,
  * storing upto 200M edges without needing to spill to disk. It provides instances of the [[algebra.Monoid]] &
  * [[org.gdget.Graph]] typeclasses.
  *
  * LabelledGraph is intended to be the goto graph implementation in gdget.
  *
  * **Note** that if our TypeTag/isInstanceOf tomfoolery does not work we can use a byte and a lookup table to do labels.
  *
  * TODO: update naive implementation to avoid having to store full edges. Can we cast from a typeTag? IF so can we
  * store neighbours like so: (Label, Map[V, Set[Label] ], Map[V, Set[Label] ]) then have a metadata map in the Graph
  * instance Map[Label, TypeTag] and then for each set lookup the typetag for each entry in each map, and create the
  * Edges of the right types (with a cast) only when we need to?
  *
  * Other option is to have E type parameter of LabelledGraph be an ADT of Labels and then have a single edge type
  * (LabelledEdge) for which takes a type parameter and a parameter of type L (the label) where L is subtype of E. This
  * would still grant the compile time safety, and would make our lives simpler.
  *
  * @see [[LabelledNeighbourhood]]
  * @author hugofirth
  */
sealed abstract class LabelledGraph[V, E] {

  import LabelledGraph._

  private[gdget] def adj: AdjacencyList[V, E]

  def size: Long
  def order: Long

  def vertices = adj.keys.iterator

  def edges = for {
    (v, (label, inEdges, outEdges)) <- adj.toIterator
    e <- inEdges
  } yield e

}

object LabelledGraph {

  type AdjacencyList[V, E] = Map[V, (Label, Map[V, BitSet], Map[V, BitSet])]

  def empty[V, E]: LabelledGraph[V, E] = ???

  final def apply[V, E](es: E*)(implicit eEv: Edge.Aux[E, V]): LabelledGraph = ???

  private[gdget] final case class GCons[V, E](adj: AdjacencyList[V, E]) extends LabelledGraph[V, E] {
    override lazy val size: Long = vertices.size
    override lazy val order: Long = edges.size
  }

  private[gdget] case object NullGraph extends LabelledGraph[Nothing, Nothing] {
    val size = 0L
    val order = 0L

    override private[gdget] val adj: AdjacencyList[Nothing, Nothing] =
      Map.empty[Nothing, (Label, Map[Nothing, BitSet], Map[Nothing, BitSet])]

    //TODO: Find out what this unapply is doing (lifted verbatim from Scalaz Map code)
    def unapply[V, E](g: LabelledGraph[V, E]): Boolean = g eq this

    def apply[V, E]: LabelledGraph[V, E] = this.asInstanceOf[LabelledGraph[V, E]]
  }
}

/**
  *
  * @param center the vertex upon which this neighbourhood is centered
  * @param label the byte label of this vertex
  * @param in
  * @param out
  * @tparam V
  */
final case class LabelledNeighbourhood[V, E](center: V, label: Label, in: Map[V, Set[E]], out: Map[V, Set[E]])


final case class LabelledEdge[L, R](source: L, label: Label, destination: R)

trait LabelledGraphInstances {
  import LabelledGraph._

}

private[gdget] sealed trait LabelledGraphLike[V, E] extends Graph[LabelledGraph, V, E] {
  import LabelledGraph._
}

private[gdget] sealed trait LabelledNeighbourhoodLike[V, E] extends Neighbourhood[LabelledGraph, V, E] {}

private[gdget] sealed trait LabelledEdgeLike[E] extends Edge[E] {}

private[gdget] sealed trait LabelledGraphMonoid[V, E] extends Monoid[LabelledGraph[V, E]]