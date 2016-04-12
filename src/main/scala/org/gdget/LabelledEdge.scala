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
package org.gdget

import scala.language.higherKinds
import scala.reflect.runtime.universe._

/** Description of Class
  *
  *
  * private trait RightFunctor[F[_,_], X] extends Functor[F[X, ?]] {
  *   implicit def F: Bifunctor[F]
  *
  *   override def map[A, C](fax: F[X, A])(f: A => C): F[X, C] = F.bimap(fax)(identity, f)
  * }
  *
  * private trait UFunctor[F[_,_]] extends Functor[λ[α => F[α, α]]] {
  *   implicit def F: Bifunctor[F]
  *
  *   override def map[A, C](fax: F[A, A])(f: A => C): F[C, C] = F.bimap(fax)(f, f)
  * }
  *
  *
  * So I'm beginning to think the inheritance goes the way I thought originally: LEdge extends Edge
  *
  * @author hugofirth
  */
trait LabelledEdge[E[+_, _, +_]] extends Any with Serializable {

  def label[L <: V, R <: V, V, Lbl](e: E[L, Lbl, R]): Lbl

  def connect[L <: V, R <: V, V, Lbl](left: L, right: R, label: Lbl): E[L, Lbl, R]

  def vertices[L <: V, R <: V, V, Lbl](e: E[L, Lbl, R]): (L, R)

  def left[L <: V, R <: V, V, Lbl](e: E[L, Lbl, R]): L

  def right[L <: V, R <: V, V, Lbl](e: E[L, Lbl, R]): R

  def other[L <: V, R <: V, V, Lbl](e: E[L, Lbl, R], v: V): Option[V]
}


object LabelledEdge {

  implicit class LabelledEdgeOps[E[_, _, _], L <: V, R <: V, V, Lbl](e: E[L, Lbl, R])(implicit val ev: LabelledEdge[E]) {

    def label = ev.label(e)
    def vertices = ev.vertices(e)
    def left = ev.left(e)
    def right = ev.right(e)
    def other(v: V) = ev.other(e, v)
  }
}