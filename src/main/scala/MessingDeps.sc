import org.gdget.collection.SimpleGraph

val a = Map(
  1 -> (Set(2,3), Set(4,5,6)),
  2 -> (Set(5,3), Set(1)),
  3 -> (Set(6,4), Set(2,1)),
  4 -> (Set(1), Set(3)),
  5 -> (Set(1), Set(2,6)),
  6 -> (Set(1,5), Set(3))
)

val b = SimpleGraph[Int](
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

