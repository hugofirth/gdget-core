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
package org.gdget.data

import org.gdget.Edge

import scala.collection.mutable
import scala.language.higherKinds

/** Description of Class
  *
  * @author hugofirth
  */
trait InMemoryGraphCompanion[G[_, _[_]]] {

  type EntryBuilder[V] = (mutable.SetBuilder[V, Set[V]], mutable.SetBuilder[V, Set[V]])
  type Entry[V] = (Set[V], Set[V])

  private[gdget] final def adjListBuilder[V, E[_]: Edge] = new mutable.Builder[E[V], Map[V, Entry[V]]] {

    val empty = mutable.HashMap.empty[V, EntryBuilder[V]]

    private var coll = empty

    override def +=(elem: E[V]): this.type = {
      val (left, right) = Edge[E].vertices(elem)
      val lN = coll.getOrElse(left, (new mutable.SetBuilder[V, Set[V]](Set.empty[V]), new mutable.SetBuilder[V, Set[V]](Set.empty[V])))
      coll.update(left, (lN._1, lN._2 += right))
      coll.update(right, (lN._1 += left, lN._2))
      this
    }

    override def clear(): Unit = coll = empty

    // TODO: Work out if there is a performance benefit of using transform -> mapResult -> result vs mapValues -> toMap?
    override def result(): Map[V, (Set[V], Set[V])] = coll.mapValues(e => (e._1.result(), e._2.result())).toMap

    //TODO: Use HashMap builder like functionality for sizeHint etc...


  }


}
