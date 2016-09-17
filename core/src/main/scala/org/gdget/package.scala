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
package org



/** Description of Class
  *
  * @author hugofirth
  */
package object gdget {

  /** Because of how we handle Labels, they should be implicitly convertible to Ints */
  type Label[A] = A => Int

  type HPair[+A] = (A, A)

  type Path[A] = Vector[A]

  /** Edge Direction ADT, in the package object because its tiny */
  sealed trait Direction
  case object In extends Direction
  case object Out extends Direction
  case object Both extends Direction
}
