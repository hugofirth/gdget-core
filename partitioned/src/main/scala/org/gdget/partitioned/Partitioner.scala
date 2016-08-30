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
  * @author hugofirth
  */
trait Partitioner[A[_]] { self =>

  /** Partition input element B and returning its PartId and the new state of the Partitioner */
  def partition[B](partitioner: A[B], input: B): (A[B], Option[PartId])

  /** Given a stream of input elements B, produce a stream of B's and their associated  partition ids, if any
    * TODO: Double check this is actually ok? My grasp of stream laziness is a little shaky
    */
  def partitionStream[B](partitioner: A[B], input: Stream[B]): Stream[(B, Option[PartId])] = input match {
    case Stream.Empty => Stream.empty[(B, Option[PartId])]
    case hd #:: tl =>
      val (dPart, pId) = self.partition(partitioner, hd)
      (hd, pId) #:: partitionStream(dPart, tl)
  }

}

object Partitioner {
  @inline def apply[A[_]: Partitioner]: Partitioner[A] = implicitly[Partitioner[A]]

  /** A couple of default instances */
  //TODO: Move these to std?

  implicit val mapPartitioner = new Partitioner[Map[?, PartId]] {
    override def partition[B](partitioner: Map[B, PartId], input: B): (Map[B, PartId], Option[PartId]) =
      (partitioner, partitioner.get(input))
  }

  implicit val partialFunPartitioner = new Partitioner[PartialFunction[?, PartId]] {
    override def partition[B](partitioner: PartialFunction[B, PartId], input: B): (PartialFunction[B, PartId], Option[PartId]) =
      (partitioner, partitioner.lift(input))
  }
}
