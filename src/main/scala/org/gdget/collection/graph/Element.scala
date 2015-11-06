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

import language.higherKinds
import scalaz.Equal

/** Utility Type class common to both vertices and edges. Elements which implement this type class are stored within a
  * Graph object.
  *
  * @author hugofirth
  * @since 0.1
  */
trait Element[A] {

  type G <: AnyRef

  //TODO: Modify this? Should it really always use universal equality in the background?
  implicit def elemEqual: Equal[A] = Equal.equalA[A]
  implicit def graphG: Graph[G]
}

object Element {

  implicit class ElementOps[A](elem: A)(implicit ev: Element[A]) {

  }
}
