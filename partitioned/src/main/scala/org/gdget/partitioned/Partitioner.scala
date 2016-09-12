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

import language.higherKinds
import scala.annotation.tailrec

/** Simple typeclass, implementations of which must take in an object (maybe a neighbourhood, vertex or edge) and return
  * a PartId
  *
  * TODO: Work out whether we want type B to be a type parameter of the typeclass or whether it is ok as a type parameter
  * on the individual methods. Turn to Cats/Scalaz for inspiration.
  *
  * @author hugofirth
  */
trait Partitioner[A, B] { self =>

  /** Partition input element B and returning its PartId and the new state of the Partitioner */
  def partition(partitioner: A, input: B): (A, Option[PartId])

  /** Given a stream of input elements B, produce a stream of B's and their associated  partition ids, if any
    * TODO: Double check this is actually ok? My grasp of stream laziness is a little shaky
    */
  def partitionStream(partitioner: A, input: Stream[B]): Stream[(B, Option[PartId])] = input match {
    case Stream.Empty => Stream.empty[(B, Option[PartId])]
    case hd #:: tl =>
      val (dPart, pId) = self.partition(partitioner, hd)
      (hd, pId) #:: partitionStream(dPart, tl)
  }

}

object Partitioner {
  @inline def apply[A, B](implicit ev: Partitioner[A, B]): Partitioner[A, B] = implicitly[Partitioner[A, B]]

  /** A couple of default instances */
  //TODO: Move these to std?

  implicit def mapPartitioner[B] = new Partitioner[Map[B, PartId], B] {
    override def partition(partitioner: Map[B, PartId], input: B): (Map[B, PartId], Option[PartId]) =
      (partitioner, partitioner.get(input))
  }

  implicit def partialFunPartitioner[B] = new Partitioner[PartialFunction[B, PartId], B] {
    override def partition(partitioner: PartialFunction[B, PartId], input: B): (PartialFunction[B, PartId], Option[PartId]) =
      (partitioner, partitioner.lift(input))
  }
}
