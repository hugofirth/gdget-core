val a = Map(
  1 -> (Set(2,3), Set(4,5,6)),
  2 -> (Set(5,3), Set(1)),
  3 -> (Set(6,4), Set(2,1)),
  4 -> (Set(1), Set(3)),
  5 -> (Set(1), Set(2,6)),
  6 -> (Set(1,5), Set(3))
)

val edges = for {
  (v, neighbours) <- a.toSeq
  in <- neighbours._1
} yield (in, v)


val b = Set(1,2,3,4)

val c = b-5

