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
package org.gdget.util

import org.gdget.labelled.Label

import language.{higherKinds, implicitConversions}

import scalaz._

/** Description of Class
  *
  * @author hugofirth
  */
//TODO: Work on implicit conversion to Having for any "evident" (Ev) instances (i.e. Element extends a typeclass Ev)
sealed trait Having[A0, TC[_]] { self =>
  type A = A0

  implicit def tc: TC[A]
  val a: A

  val label: Symbol

  override def toString = a.toString

  /** @inheritdoc
    *
    * Having wrappers are designed to go in heterogeneous collections. Therefore, we need to use == as well
    * as provide an Equal[ Having[_, _] ] instance.
    *
    * Semantically, Having is a completely transparent wrapper type, so equality is defined solely by A's equality.
    */
  override def equals(obj: Any): Boolean = obj match {
    case e: Having[_, _] if self eq e => true
    case e Has _ => self.a == e
    case _ => false
  }
}

final case class Has[Lbl[_] <: Label[_], A0 : Lbl, TC[_]](a: A0, tc: TC[A0]) extends Having[A0, TC] {
  override val label = implicitly[Lbl[A]].name
}

object Having {

  /*Typeclass instances */
  implicit def havingEqual[TC[_] , A: Equal]: Equal[Having[A, TC]] = new Equal[Having[A, TC]] {
    override def equalIsNatural = Equal[A].equalIsNatural
    override def equal(h1: Having[A, TC], h2: Having[A, TC]) = Equal[A].equal(h1.a, h2.a)
  }

  implicit def havingShow[TC[_], A: Show]: Show[Having[A, TC]] = new Show[Having[A, TC]] {
    override def show(h: Having[A, TC]) = s"${Show[A].show(h.a)} (has a ${h.tc.getClass.getCanonicalName})"
  }
}






