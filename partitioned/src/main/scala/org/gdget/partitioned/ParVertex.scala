/** gdget
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
package org.gdget.partitioned


/** A simple typeclass for vertex types which ensures that they can return an PartId for the partition they belong to.
  *
  * @author hugofirth
  */
trait ParVertex[V] extends Any with Serializable {

  def partition(v: V): Option[PartId]

}

object ParVertex {

  @inline def apply[V: ParVertex]: ParVertex[V] = implicitly[ParVertex[V]]

  //TODO: Move instances to std pacakge - this is just a quick fix
  //TODO: Use Discipline/Scalacheck/Scalatest (Discpline?) to create a simple law which checks for same eq/hashcode with different partitions
  //This would make the below a lawless instance, but thats ok.
  implicit def tupleParVertex[V] = new ParVertex[(V, PartId)] {
    override def partition(v: (V, PartId)): Option[PartId] = Option(v._2)
  }
}
