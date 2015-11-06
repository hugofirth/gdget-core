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

import language.higherKinds

import scalaz._
import Scalaz._

/** Description of Class
  *
  * @author hugofirth
  */
trait Having[A0, TC[_]] {
  type A = A0

  implicit def tc: TC[A]

  val a: A
  override def toString = a.toString
}

case class Has[A0, TC[_]](a: A0, tc: TC[A0]) extends Having[A0, TC]

object Having {
  def unapply[A, TC[_]](elem: Having[A, TC]): Option[(elem.A, TC[elem.A])] = Option((elem.a, elem.tc))

  implicit def havingEqual[TC[_] , A](implicit ev: Equal[A]): Equal[Having[A, TC]] = new HavingEqual[A, TC] {
    override implicit def a: Equal[A] = ev
  }
}

private trait HavingEqual[A, TC[_]] extends Equal[Having[A, TC]] {
  implicit def a: Equal[A]

  final override def equal(h1: Having[A, TC], h2: Having[A, TC]) = a.equal(h1.a, h2.a)

  override val equalIsNatural = a.equalIsNatural
}





