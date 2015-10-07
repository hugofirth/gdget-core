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
package org.gdget.collection.graph.immutable

import language.higherKinds

/** Description of Class
  *
  * @author hugofirth
  */
case class Graph(vertices: Set[InnerVertex[_]], edges: Set[InnerEdge[_, _, _]])


//TODO: Finish factory method, implement ExternalElement type and toInner(G), toOuter methods on ElementOps classes
object Graph {
   def factory(edges: OuterEdge[_, _, _]*): Graph = {
//    for {
//      e <- edges
//      v <- (e.left, e.right)
//    } yield { }
    ???
   }

//  implicit def graphLikeGraph[]
}
