//import org.gdget.{Edge, Vertex}
import org.gdget.labelled.Label

import language.{higherKinds, existentials}
object Example {
  /** Vertex types */
  case class Foo(value: Int)
  case class Bar(value: Int)
  case class Baz(value: Int)

  /** Vertex TC instances */
  /** Edge types */
  case class IEdge[L, R](lhs: L, rhs: R)
  /** Declare Vertex Label Set */
  sealed trait VertexLabel[V] extends Label[V]
  object VertexLabel {

    implicit object FooLabel extends VertexLabel[Foo] {
      override def name = 'Foo
    }

    implicit object BarLabel extends VertexLabel[Bar] {
      override def name = 'Bar
    }

  }

  /** Declare another Vertex Label Set */
  sealed trait OtherVertexLabel[V] extends Label[V]
  object OtherVertexLabel {
    implicit object BazLabel extends OtherVertexLabel[Baz] {
      override def name = 'Baz
    }

    implicit object FooLabel extends OtherVertexLabel[Foo] {
      override def name = 'Foo
    }
  }



  /** Declare Edge Label Set */
  sealed trait EdgeLabel[E] extends Label[E]
  object EdgeLabel {

    implicit def IEdgeLabel[L, R]  = new EdgeLabel[IEdge[L, R]] {
      override def name = 'Edge
    }
  }
  case class Graph[Lv[_] <: Label[_], Le[_] <: Label[_]](vertices: Set[Any], edges: Set[Any])
  def addVertex[Lv[_] <: Label[_], +Le[_] <: Label[_], V : Lv](g: Graph[Lv, Le], v: V) = g.copy(vertices = g.vertices + v)
  val g = Graph[VertexLabel, EdgeLabel](vertices = Set.empty[Any], edges = Set.empty[Any])
  val v = Foo(1)
  val v2 = Baz(2)
  val g2 = addVertex(g, v)
  val g3 = addVertex(g2, Foo(2))
  val gOther = Graph[VertexLabel with OtherVertexLabel, EdgeLabel](vertices = Set.empty[Any], edges = Set.empty[Any])


  val g4 = addVertex(gOther, v2)



}