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

/** This utility type is what is known as an existential quantification. It means "there exists an F[A] for A".
  *
  *  Credit to @tpolecat (Rob Norris) and his source (@nuttycom) for
  *  [[https://tpolecat.github.io/2015/04/29/f-bounds.html this article]], which is where I learned about the technique.
  *
  * @author hugofirth
  * @since 0.1
  */
trait Exists[F[_]] {
  type A
  val a: A
  val fa: F[A]
  override def toString = a.toString
}

object Exists {
  def apply[F[_], A0](a0: A0)(implicit ev: F[A0]): Exists[F] = new Exists[F] {
    type A = A0
    val a = a0
    val fa = ev
  }
}
