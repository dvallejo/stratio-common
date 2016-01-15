/**
  * Copyright (C) 2015 Stratio (http://stratio.com)
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  * http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
  */

package com.stratio.common.utils.components.dao

import com.stratio.common.utils.components.config.impl.TypesafeConfigComponent
import com.stratio.common.utils.components.logger.impl.SparkLoggerComponent
import com.stratio.common.utils.components.repository.impl.ZookeeperRepositoryComponent
import org.json4s.DefaultFormats
import org.json4s.jackson.Serialization._

trait GenericDAOComponent[M <: AnyRef] extends DAOComponent[String,Array[Byte],M]
with ZookeeperRepositoryComponent with TypesafeConfigComponent with SparkLoggerComponent {

  val dao: DAO = new GenericDAO()

  class GenericDAO(key: Option[String] = None) extends DAO {

    implicit val formats = DefaultFormats

    def entity: String = {
      if(key.isEmpty || key.get.trim.isEmpty) throw new IllegalStateException("EntityName in the DAO must be defined")
      else key.get
    }

    override def fromVtoM(v: Array[Byte])(implicit manifest: Manifest[M]): M = read[M](new String(v))
    override def fromMtoV(m: M)(implicit manifest: Manifest[M]): Array[Byte] = write(m).getBytes
  }
}