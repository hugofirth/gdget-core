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
package org.gdget.labelled

/** Description of Class
  *
  * @author hugofirth
  */
trait LabelledEdge {
//  trait Edge[E[_, _], L, R] extends Element[E[L, R]] {
//
//    type V[L, R] = t[L]#t[R]
//
//    def vertices[L, R](e: E[L, R])(implicit lEr: Vertex[L], rEr: Vertex[R]): (L, R)
//
//    def other[L: Vertex, R: Vertex, A:({type λ[a] = a In V[L, R]})#λ](e: E[L, R], v: A): Option[L\/R]
//
//    def left[L, R](e: E[L, R])(implicit lEr: Vertex[L], rEr: Vertex[R]): L
//
//    def right[L, R](e: E[L, R])(implicit lEr: Vertex[L], rEr: Vertex[R]): R
//  }
//
//  object Edge {
//
//    implicit class EdgeOps[E[_, _], L, R](e: E[L, R])(implicit eEv: Edge[E], lEr: Vertex[L], rEr: Vertex[R]) {
//      def vertices = eEv.vertices(e)
//      def left = eEv.left(e)
//      def right = eEv.right(e)
//      def other[A](v: A)(implicit ev: A In Edge[E]#V[L, R]) = eEv.other(e, v)
//    }
//
//  }
}
