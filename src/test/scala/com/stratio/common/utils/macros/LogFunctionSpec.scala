/*
 * Copyright (C) 2015 Stratio (http://stratio.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.stratio.common.utils.macros

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito.verify
import org.slf4j.Logger

@RunWith(classOf[JUnitRunner])
class LogFunctionSpec extends WordSpec with Matchers with MockitoSugar {

  trait MockLogger {

    val logger = mock[Logger]
  }

  "A LogFunction Macro" when {
    "a method with arguments is called" should {
      "send a trace log message with info about the call" in new MockLogger {

        @LogFunction
        def example(a: String, b: Int): String = a + b.toString

        example("hello", 1)
        verify(logger).trace("Calling method 'example' with arguments: a = hello, b = 1")
      }
    }

    "a method with several argument sets is called" should {
      "send a trace log message with info about the call" in new MockLogger {

        @LogFunction
        def example(a: String)(b: Int): String = a + b.toString

        example("hello")(1)
        verify(logger).trace("Calling method 'example' with arguments: a = hello, b = 1")
      }
    }
  }

}
