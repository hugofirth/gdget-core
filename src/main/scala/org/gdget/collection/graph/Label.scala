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
package org.gdget.collection.graph

import language.implicitConversions


/** The Label utility trait, implemented by any Type to be contained by vertices or edges
  *
  * @tparam A
  * @author hugofirth
  * @since 0.1
  */
trait Label[A] { self =>

  def name: String
  override def toString = self.name
}

/** Pi type test for Labels
  *
  * TODO: Finish documenting here
  *
  * @author hugofirth
  */
trait ConstLabel {
  type Type
  val label: Label[Type]
  val value: Type
}

object ConstLabel {

  implicit def fromLabel[A](a: A)(implicit lbl: Label[A]): ConstLabel = new ConstLabel {
    override type Type = A
    override val value: Type = a
    override val label: Label[Type] = lbl
  }
}
