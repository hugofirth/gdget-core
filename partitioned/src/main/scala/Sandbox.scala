import cats.data.WriterT
import cats.functor.Bifunctor
import org.gdget.partitioned._
import org.gdget.data.query._
import org.gdget.{Edge, Graph}
import org.gdget.std.all._

import language.higherKinds
import scala.concurrent._

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
/** Description of Class
  *
  * @author hugofirth
  */
object Sandbox extends App {

  //TODO: Finish Partitioner implementation so that it can be passed to LogicalParGraph apply
  val a = Map(
    1 -> 1.part,
    2 -> 1.part,
    3 -> 1.part,
    4 -> 2.part,
    5 -> 2.part,
    6 -> 2.part
  )

  type UTuple[A] = (A, A)

  val b: LogicalParGraph[Int, UTuple] = LogicalParGraph[Int, UTuple](
    1 -> 4,
    1 -> 5,
    1 -> 6,
    2 -> 1,
    3 -> 2,
    3 -> 1,
    4 -> 3,
    5 -> 2,
    5 -> 6,
    6 -> 3
  )

  import cats.syntax.traverse._
  import cats._
  import cats.std.all._
  import ExecutionContext.Implicits.global

  import LogicalParGraph._

  implicitly[Edge[UTuple]]
  implicitly[Monad[Either[String, ?]]]
  implicitly[Bifunctor[WriterT[Option, ?, ?]]]
//  implicitly[Graph[LogicalParGraph[Map[?, PartId], ?, ?[_]]]]

//  def test[S[_]: ParScheme]: Graph[LogicalParGraph[S, ?, ?[_]]] = Graph[LogicalParGraph[S, ?, ?[_]]]

//  implicit val gimp = test[Scheme]

  //Todo - make op object which means you only have to declare types once.
  //E.G. val op = QueryBuilder[G, V, E]
  // op.get(1)
  // op.traverseEdge(...)
  // If we think op.blah is cumbersome, then how about ...?

  //TODO: Use kleisli composition to avoid having to flatten at the end?

  //TODO: What about a Queryable function which takes a Graph and a ParScheme. Perhaps also an implicit QueryBuilder
  //  which I could then use to prop up type inference?

  def query: QueryIO[LogicalParGraph, Int, UTuple, Option[(Int, Int)]] = {
    for {
      v <- get[LogicalParGraph, Int, UTuple](1)
      p <- v.traverse(traverseEdge[LogicalParGraph, Int, UTuple](_, (1, 4)))
    } yield p.flatten
  }

  val result = query.transK[Future].run(b)
  result.onSuccess {
    case Some(edge) => println(s"Result is: $edge")
    case None => println("The query returns nothing")
  }


}
