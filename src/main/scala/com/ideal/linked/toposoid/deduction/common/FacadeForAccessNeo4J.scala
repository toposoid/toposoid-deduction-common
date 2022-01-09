/*
 * Copyright 2021 Linked Ideal LLC.[https://linked-ideal.com/]
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.ideal.linked.toposoid.deduction.common

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpMethods, HttpRequest}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import play.api.libs.json.Json
import com.ideal.linked.common.DeploymentConverter.conf
import com.typesafe.scalalogging.LazyLogging

import scala.util.{Failure, Success, Try}

/**
 * Common functions used for microservices using Neo4J
 */
object FacadeForAccessNeo4J extends LazyLogging{

  def getCypherQueryResult(query:String, target:String): String = Try{
    val retryNum =  conf.getInt("retryCallMicroserviceNum") -1
    for (i <- 0 to retryNum) {
      val result:String  = this.getCypherQueryResultImpl(query, target)
      if (result != "{}") {
        return result
      }
      if(i == retryNum) throw new Exception("Results were not returned properly")
    }
    ""
  }match {
    case Success(s) => s
    case Failure(e) => throw e
  }


  /**
   * This function throws a query to the microservice and returns the result as Json
   * @param query
   */
  private def getCypherQueryResultImpl(query:String, target:String): String = {

    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher
    implicit val jsonStreamingSupport: JsonEntityStreamingSupport = EntityStreamingSupport.json()
    val input = """{ "query":"%s", "target":"%s" }""".format(query, target)
    val entity = HttpEntity(ContentTypes.`application/json`, input)
    val url = "http://" + conf.getString("scala-data-accessor-neo4j-web.address") + ":" + conf.getString("scala-data-accessor-neo4j-web.port") + "/getQueryFormattedResult"
    val request = HttpRequest(uri = url, method = HttpMethods.POST, entity = entity)

    val result = Http().singleRequest(request)
      .flatMap { res =>
        Unmarshal(res).to[String].map { data =>
          Json.parse(data.getBytes("UTF-8"))
        }
      }
    var queryResultJson:String = "{}"
    result.onComplete {
      case Success(js) =>
        //println(s"Success: $js")
        queryResultJson = s"$js"
        logger.info(s"Success: $js")
      case Failure(e) =>
        //println(s"Failure: $e")
        logger.error(s"Failure: $e")
    }
    while(!result.isCompleted){
      Thread.sleep(20)
    }
    queryResultJson
  }
}
