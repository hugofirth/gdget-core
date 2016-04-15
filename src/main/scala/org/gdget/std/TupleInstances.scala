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
package org.gdget.std

import language.reflectiveCalls

import cats._
import cats.syntax.eq._

import org.gdget.Edge

/** Description of Class
  *
  * @author hugofirth
  */
trait TupleInstances {

  //TODO: Remove this once Cats rolls in TupleN instances for Algrebra (cats-kernel) typeclasses
  implicit def tuple2Eq[A: Eq, B: Eq]: Eq[(A, B)] = new Eq[(A, B)] {
    def eqv(x: (A, B), y: (A, B)) = x._1 === y._1 && x._2 === y._2
  }


  implicit def tuple2Edge: Edge[({ type λ[a] = (a, a) })#λ] = new Edge[({ type λ[a] = (a, a) })#λ] {

    override def connect[V](left: V, right: V, label: Unit = ()): (V, V) = (left, right)

    override def left[V](e: (V, V)): V = e._1

    override def right[V](e: (V, V)): V = e._2

    override def other[V](e: (V, V), v: V): Option[V] =
      if(e._1 == v)
        Option(e._2)
      else if(e._2 == v)
        Option(e._1)
      else
        None

    override def vertices[V](e: (V, V)) = e

  }

}
