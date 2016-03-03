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

import org.gdget.Graph

import language.{higherKinds, reflectiveCalls}

/** Description of Class
  *
  * @author hugofirth
  */
trait LabelledGraph[G[_, _, _[_] <: Label[_], _[_] <: Label[_]], V, E, Lv[_] <: Label[_], Le[_] <: Label[_]] extends
  Graph[({type λ[v, e] = G[v, e, Lv, Le] })#λ, V, E]{

  //TODO: Look at merge-able Union types / Label sets
  //  def union[LeftLv[_] <: Label[_],
  //            LeftLe[_] <: Label[_],
  //            RightLv[_] <: Label[_],
  //            RightLe[_] <: Label[_]](lg: G[LeftLv, LeftLe], rg: G[RightLv, LeftLe]): G[LeftLv with RightLv, LeftLe with RightLe]
  //
  //  def intersection(lg: G[_, _], rg: G[_, _]): G[Label, Label]

  //TODO: Look at making a typeSafe variant of asInstanceOf, a la shapeless.
  //  def findVertex[Lv[_] <: Label[_],
  //  Le[_] <: Label[_],
  //  V : Vertex : Lv](g: G[Lv, Le])(f: (V) => Boolean): Option[V] = {
  //    val label = implicitly[Lv[V]].name
  //    vertices(g) collect { case v if v.label == label => v } map (_.asInstanceOf[V]) find f
  //  }
  //
  //  //Should I use a context bound [A : B] ==> (implicit ev: B[A]) for L & R here? L : Vertex : Lv ??
  //  def findEdge[Lv[_] <: Label[_],
  //  Le[_] <: Label[_],
  //  E[_, _]: Edge : Lv, L, R](g: G[Lv, Le])(f: E[L, R] => Boolean): Option[E[L, R]] = {
  //    val label = implicitly[Le[E]].name
  //    edges(g) collect { case e if e.label == label => e } map (_.asInstanceOf[E[L, R]]) find f
  //  }

}
