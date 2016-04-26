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
package org.gdget.data

import cats.free.Free.liftF

/** The ADT representing a basic grammar for read-only queries over collection types which provide [[org.gdget.Graph]]
  * instances.
  *
  * @author hugofirth
  */
sealed trait QueryA[A]
//Get Vertex
//WithEdgeFrom
//WithEdgeTo

case class GetMatch[T](patternGraph: T) extends QueryA[List[T]]
case class Where[T](filter: T => Boolean) extends QueryA[List[T]]

object Query {

  def getMatch[T](patternGraph: T): Query[List[T]] = liftF[QueryA, List[T]](GetMatch[T](patternGraph))

  def where[T](filter: T => Boolean): Query[List[T]] = liftF[QueryA, List[T]](Where[T](filter))

}