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
package org.gdget

import language.higherKinds

/** Utility Type class common to both vertices and edges. Elements which implement this type class are stored within a
  * Graph object.
  *
  * @author hugofirth
  * @since 0.1
  */
trait Element[A] {

  /** Each Element instance must define a G supertype which has an instance of Graph */
//  type G[_, _]
//  implicit def graphG: Graph[G, _, _]
//
//  def graph(elem: A): G

}

object Element {

//  implicit class ElementOps[A: Element](elem: A) {
//    private val eEv = implicitly[Element[A]]
//    def graph = eEv.graph(elem)
//  }
}
