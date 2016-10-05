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

import cats.Foldable

import language.higherKinds

/** Simple typeclass, implementations of which must take in an object (maybe a neighbourhood, vertex or edge) and return
  * a PartId
  *
  * @author hugofirth
  */
trait Partitioner[A, B, -C, F[_]] { self =>

  implicit def F: Foldable[F]

  /** Partition input element B and returning it with a partId and the new state of the Partitioner */
  def partition[CC <: C](partitioner: A, input: B, context: CC): (A, F[(B, PartId)])

}

object Partitioner {
  @inline def apply[A, B, C, F[_]: Foldable](implicit ev: Partitioner[A, B, C, F]): Partitioner[A, B, C, F] =
    implicitly[Partitioner[A, B, C, F]]

  /** A couple of default instances */
  //TODO: Move these to std?

  implicit def mapPartitioner[B] = new Partitioner[Map[B, PartId], B, Unit, Option] {

    /** Partition input element B and returning it with a partId and the new state of the Partitioner */
    override def partition[CC <: Unit](partitioner: Map[B, PartId], input: B,
                                    context: CC): (Map[B, PartId], Option[(B, PartId)]) = {
      (partitioner, partitioner.get(input).map((input, _)))
    }

    override implicit def F: Foldable[Option] = Foldable[Option]
  }

  implicit def partialFunPartitioner[B] = new Partitioner[PartialFunction[B, PartId], B, Unit, Option] {

    override implicit def F: Foldable[Option] = Foldable[Option]

    /** Partition input element B and returning it with a partId and the new state of the Partitioner */
    override def partition[CC <: Unit](partitioner: PartialFunction[B, PartId], input: B,
                                    context: CC): (PartialFunction[B, PartId], Option[(B, PartId)]) = {
      (partitioner, partitioner.lift(input).map((input, _)))
    }
  }

}
