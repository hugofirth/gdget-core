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
import language.higherKinds

case class Foo[+A](a: A)

class Bar(b: Int) {
  type T
}

type Aux[A, B] = B {type T = A}

case class Baz[A, B](c: B)

val a: Foo[Int] = Foo(2)
val b = new Bar(4) { type T = Int }
Baz[Int, Bar](b)

