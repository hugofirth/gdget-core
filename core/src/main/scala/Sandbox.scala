import org.gdget.{Edge, Neighbourhood}

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

  import org.gdget.Graph
  import org.gdget.data._
  import org.gdget.std.all._
  import language.higherKinds

  val a = Map(
    1 -> (Set(2,3), Set(4,5,6)),
    2 -> (Set(5,3), Set(1)),
    3 -> (Set(6,4), Set(2,1)),
    4 -> (Set(1), Set(3)),
    5 -> (Set(1), Set(2,6)),
    6 -> (Set(1,5), Set(3))
  )

  type UTuple[A] = (A, A)

  val b = SimpleGraph[Int, UTuple](
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


  implicitly[Edge[UTuple]]

  import SimpleGraph._

  val op = new GraphOp[SimpleGraph, Int, UTuple](b)



  //TODO: Work out why I have to manually annotate Tuple2[Int, Int] with its E[Int] alias?

  def query: QueryIO[SimpleGraph, Int, UTuple, Option[UTuple[Int]]] =
    for {
      v <- op.get(1)
      p <- op.traverseEdge(v.get, (1, 4))
    } yield p


  import cats.{~>, Id}


  def toyInterpreter = new (QueryOp[SimpleGraph, Int, UTuple, ?] ~> Id) {
    override def apply[A](fa: QueryOp[SimpleGraph, Int, UTuple, A]): Id[A] =
      fa match {
        case Get(v, g) => {
          println(s"Get $v")
          Graph[SimpleGraph].getVertex(g, v)
        }
        case GetWhere(condition, g) => Graph[SimpleGraph].vertices(g).filter(condition).toList
        case TraverseEdge(v, e, g) => {
          println(s"Traverse edge $e of $v")
          val n = Graph[SimpleGraph].neighbourhood(g, v)
          n.fold(None)(nei => Graph[SimpleGraph].N.edges(nei).find(_ == e))

        }
        case TraverseInNeighbour(v, inV, g) => ???
        case TraverseOutNeighbour(v, outV, g) => ???
      }
  }

  def interpreter[G[_, _[_]], V, E[_]](implicit gEv: Graph[G], eEv: Edge[E]) = new (QueryOp[G, V, E, ?] ~> Id) {
    def apply[A](fa: QueryOp[G, V, E, A]): Id[A] =
      fa match {
        case Get(v, g) => ???
        case GetWhere(condition, g) => ???
        case TraverseEdge(vs, e, g) => ???
        case TraverseInNeighbour(v, inV, g) => ???
        case TraverseOutNeighbour(v, outV, g) => ???
      }
  }



}
