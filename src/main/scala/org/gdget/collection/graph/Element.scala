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

trait Element[A[_], B] {

  implicit def valueEqual: Equal[B] = Equal.equalA[B]

  def get(elem: A[B]): B
}

object Element {

  implicit class ElementOps[A[_], B](elem: A[B])(implicit ev: Element[A, B]) {

    def get = ev.get(elem)
  }
}

trait ExternalElement[A[_], B] extends Element[A, B] {

  type Inner[G]

  def toInner[G](elem: A[B], graph: G)(implicit ev: InternalElement[Inner[G], B, G], gEv: GraphLike[G, _, _]): Inner[G]
}

object ExternalElement {

  implicit class ExternalelementOps[A[_], B](elem: A[B])(implicit ev: ExternalElement[A, B]) {

    def toInner[G](graph: G)(implicit iEv: InternalElement[ev.Inner[G], B, G], gEv: GraphLike[G, _, _]) =
      ev.toInner(elem, graph)
  }
}

/** Utility Type class common to both vertices and edges. Elements which implement this type class are stored within a
  * Graph object.
  *
  * @author hugofirth
  */
trait InternalElement[A[_], B, G] extends Element[A, B] {

  type Outer

  def graph(elem: A[B])(implicit gEv: GraphLike[G, _, _]): G

  def toOuter(elem: A[B])(implicit ev: ExternalElement[Outer, B]): Outer
}


object InternalElement {

  implicit class InternalElementOps[A[_], B, G](elem: A)(implicit ev: InternalElement[A, B, G]) {

    def toOuter(implicit eEv: ExternalElement[ev.Outer, B]) = ev.toOuter(elem)

    def graph(implicit gEv: GraphLike[G, _, _]): G = ev.graph(elem)
  }
}
