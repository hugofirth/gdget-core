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

import language.implicitConversions

/** A generalisation of Scala's Either type to an arbitrary number of possible types.
  *
  *   Lifted from the excellent article by eed3si9n [[http://eed3si9n.com/learning-scalaz/Coproducts.html]]
  *
  *   Port of below removed from Scalaz circa 7.2.x for unknown reason
  *   ([[https://github.com/scalaz/scalaz/commit/9f4c39816940c42c472cfbce48a1d2b72b22d9f2 commit]]). Available in Shapeless as
  *   [[https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0#coproducts-and-discriminated-unions Coproduct]]
  *
  * @author hugofirth
  * @since 0.1
  */
trait UnionTypes {

  type ![A] = A => Nothing
  type !![A] = ![![A]]

  trait Disj { self =>
    type D
    type t[S] = Disj {
      type D = self.D with ![S]
    }
  }

  type t[T] = {
    type t[S] = (Disj { type D = ![T] })#t[S]
  }

  type or[T <: Disj] = ![T#D]

  type In[S, T <: Disj] = !![S] <:< or[T]

  sealed trait Union[T] {
    val value: Any
  }

  case class Converter[S](s: S) {
    def union[T <: Disj](implicit ev: In[S, T]): Union[T] =
      new Union[T] {
        val value = s
      }
  }

  implicit def any2Converter[S](s: S): Converter[S] = Converter[S](s)

}

object UnionTypes extends UnionTypes

