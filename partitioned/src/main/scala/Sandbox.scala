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
  val (v1, v2, v3, v4, v5, v6) = (1 -> 1.part, 2 -> 1.part, 3 -> 1.part, 4 -> 2.part, 5 -> 2.part, 6 -> 2.part)

  type UTuple[A] = (A, A)

  val b: LogicalParGraph[(Int, PartId), UTuple] = LogicalParGraph[(Int, PartId), UTuple](
    v1 -> v4,
    v1 -> v5,
    v1 -> v6,
    v2 -> v1,
    v3 -> v2,
    v3 -> v1,
    v4 -> v3,
    v5 -> v2,
    v5 -> v6,
    v6 -> v3
  )

  import cats.syntax.traverse._
  import cats._
  import cats.instances.all._
  import ExecutionContext.Implicits.global
  import LogicalParGraph._





  //TODO: Use kleisli composition to avoid having to flatten at the end?

  //TODO: What about a Queryable function which takes a Graph and a ParScheme. Perhaps also an implicit QueryBuilder
  //  which I could then use to prop up type inference?

  def query = {
    val op = QueryBuilder[LogicalParGraph, (Int, PartId), UTuple]
    for {
      v <- op.get(v1)
      p <- v.traverse(op.traverseEdge(_, (v1, v4)))
    } yield p.flatten
  }

  val result = query.transK[Future].run(b)
  result.onSuccess {
    case Some(edge) => println(s"Result is: $edge")
    case None => println("The query returns nothing")
  }


}
