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
package org.gdget.collection.mutable

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

/** A custom implementation of Map[A, B] based upon a Patricia Trie.
  *
  *   This class allows you to fetch the subset of values mapped to keys with a particular prefix.
  *   It also allows you to query the longest prefix of a given `Seq[A]` which exists within the map's key set.
  *
  *   {{{
  *   val trie = TrieMap.empty[Char, Int]
  *
  *   trie += (("abd", 0))
  *   trie += (("acd", 1))
  *   trie += (("aca", 2))
  *   trie += (("cca", 3))
  *
  *   val subTrie = trie withPrefix "a"
  *   // subTrie ==
  *   //    TrieMap(
  *   //      Vector('b', 'd') -> 0,
  *   //      Vector('c', 'd') -> 1,
  *   //      Vector('c', 'a') -> 2
  *   //    )
  *
  *   val mutual = trie maxMutualPrefix "aced"
  *   // mutual == Vector('a', 'c')
  *   }}}
  *
  * @author hugofirth
  * @since 0.1
  */
class TrieMap[A, B] extends mutable.Map[Seq[A],B] with mutable.MapLike[Seq[A],B, TrieMap[A, B]] {

  private var suffixes: Map[A, TrieMap[A, B]] = Map.empty
  var value: Option[B] = None

  final def withPrefix(prefix: Seq[A]): TrieMap[A, B] = {
    if (prefix.isEmpty) {
      this
    } else {
      val leading = prefix.head
      suffixes get leading match {
        case None =>
          suffixes = suffixes + (leading -> empty)
        case _ =>
      }
      suffixes(leading) withPrefix prefix.tail
    }
  }

  @tailrec
  final def maximumMutualPrefix(remaining: Seq[A], prefix: Seq[A] = Seq.empty[A]): Seq[A] =
    suffixes get remaining.head match {
      case Some(subTrie) => subTrie.maximumMutualPrefix(remaining.tail, prefix:+remaining.head)
      case _ => prefix
    }


  override def get(key: Seq[A]): Option[B] = {
    if(key.isEmpty) { value } else { suffixes get key.head flatMap( _ get key.tail ) }
  }

  override def update(key: Seq[A], value: B): Unit = withPrefix(key).value = Some(value)

  override def remove(key: Seq[A]): Option[B] = {
    if(key.isEmpty) {
      val previous = value; value = None; previous
    } else {
      suffixes get key.head flatMap( _ remove key.tail )
    }
  }

  override def +=(kv: (Seq[A], B)): this.type = { update(kv._1, kv._2); this }

  override def -=(key: Seq[A]): this.type = { remove(key); this }

  override def iterator: Iterator[(Seq[A], B)] = {
    (for(m <- value.iterator) yield (IndexedSeq.empty[A], m)) ++
      (for {
        (symbol, subTries) <- suffixes.iterator
        (pattern, patterns) <- subTries.iterator
      } yield (symbol +: pattern, patterns))
  }

  override def empty = new TrieMap[A, B]

}

object TrieMap {

  def apply[A,B](mappings: (Seq[A],B)*): TrieMap[A,B] = {
    val trie = new TrieMap[A,B]
    mappings foreach ( trie += _ )
    trie
  }

  def empty[A, B] = new TrieMap[A, B]

  def newBuilder[A, B]: mutable.Builder[(Seq[A],B), TrieMap[A, B]] =
    new mutable.MapBuilder[Seq[A], B, TrieMap[A, B]](empty)

  implicit def canBuildFrom[A, B]: CanBuildFrom[TrieMap[_,_], (Seq[A], B), TrieMap[A, B]] = {
    new CanBuildFrom[TrieMap[_,_], (Seq[A], B), TrieMap[A, B]] {
      def apply(from: TrieMap[_,_]) = newBuilder[A, B]
      def apply() = newBuilder[A, B]
    }
  }
}

/** A trait for mutable [[org.gdget.collection.mutable.TrieMap]]s with multiple values assigned to a single prefix.
  *
  *  This trait is typically used as a mixin. It turns TrieMaps which map `A` to `Set[B]` objects into multimaps that
  *  map `A` to `B` objects.
  *
  *  It functions similarly to [[scala.collection.mutable.MultiMap]]; the sole difference in their behaviours is that a
  *  value `B` bound to a given sequence `Seq[A]` is also bound to every prefix of `Seq[A]` in the TrieMap's key set.
  *
  *  {{{
  *  val multi = new TrieMap[Char, Set[Int]] with TrieMultiMap[Char, Int]
  *
  *  multi addBinding("ab", 1)
  *  multi addBinding("abc", 2)
  *
  *  val values = multi get 'a'
  *  // values == Set(1,2)
  *  }}}
  *
  * @author hugofirth
  * @since 0.1
  */
trait TrieMultiMap[A, B] extends TrieMap[A, mutable.Set[B]] with mutable.MultiMap[Seq[A], B] {

  override final def addBinding(key: Seq[A], valueMember: B): this.type = {
    recursiveAddBinding(this, key, valueMember)
    this
  }

  @tailrec
  private def recursiveAddBinding(trieMultiMap: TrieMap[A,mutable.Set[B]], key: Seq[A], valueMember: B): Unit = {
    trieMultiMap.value = trieMultiMap.value match {
      case Some(set) => Some(set += valueMember)
      case None => Some(mutable.Set(valueMember))
    }
    if(key.nonEmpty) recursiveAddBinding(trieMultiMap withPrefix (key take 1), key.tail, valueMember)
  }

  override final def removeBinding(key: Seq[A], valueMember: B): this.type = {
    recursiveRemoveBinding(this, key, valueMember)
    this
  }

  @tailrec
  private def recursiveRemoveBinding(trieMultiMap: TrieMap[A,mutable.Set[B]], key: Seq[A], valueMember: B): Unit = {
    trieMultiMap.value map( _ remove valueMember )
    if(key.nonEmpty) recursiveRemoveBinding(trieMultiMap withPrefix (key take 1), key.tail, valueMember)
  }

  override def empty =  new TrieMap[A, mutable.Set[B]] with TrieMultiMap[A, B]

}