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
import com.ideal.linked.toposoid.common.{CLAIM, PREMISE, ToposoidUtils}
import com.ideal.linked.toposoid.knowledgebase.model.{KnowledgeBaseEdge, KnowledgeBaseNode, LocalContext}
import com.ideal.linked.toposoid.protocol.model.base.{AnalyzedSentenceObject, AnalyzedSentenceObjects, DeductionResult}
import com.ideal.linked.toposoid.protocol.model.neo4j.{Neo4jRecordMap, Neo4jRecords}
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
   *
   * @param propositionId
   * @return
   */
  def havePremiseNode(propositionId:String):Boolean = Try{
    val query = "MATCH (m:PremiseNode)-[e:LogicEdge]-(n:ClaimNode) WHERE n.propositionId='%s' return m, e, n".format(propositionId)
    val jsonStr:String = getCypherQueryResult(query, "")
    if(jsonStr.equals("""{"records":[]}""")) false
    else true
  }match {
    case Success(s) => s
    case Failure(e) => throw e
  }

  /**
   *
   * @param propositionId
   * @param sentenceType
   * @return
   */
  def neo4JData2AnalyzedSentenceObjectByPropositionId(propositionId:String, sentenceType:Int):AnalyzedSentenceObjects = Try{
    val nodeType:String = ToposoidUtils.getNodeType(sentenceType)
    val query = "MATCH (n1:%s)-[e]->(n2:%s) WHERE n1.propositionId='%s' AND n2.propositionId='%s' RETURN n1, e, n2".format(nodeType, nodeType, propositionId, propositionId)
    val jsonStr:String = getCypherQueryResult(query, "")
    //If there is even one that does not match, it is useless to search further
    val neo4jRecords:Neo4jRecords = Json.parse(jsonStr).as[Neo4jRecords]

    val neo4jDataInfo = neo4jRecords.records.foldLeft(Map.empty[String, (Map[String, KnowledgeBaseNode], List[KnowledgeBaseEdge])]){
      (acc, x) =>{
        val node1:KnowledgeBaseNode = x(0).value.logicNode
        val node2:KnowledgeBaseNode = x(2).value.logicNode

        val key = node1.nodeId.substring(0, node1.nodeId.lastIndexOf("-"))
        val key2 = node2.nodeId.substring(0, node2.nodeId.lastIndexOf("-"))
        if (!key.equals(key2)) {
          acc
        }else{
          val knowledgeBaseNode1 = KnowledgeBaseNode(
            nodeId = node1.nodeId,
            propositionId = node1.propositionId,
            sentenceId = node1.sentenceId,
            predicateArgumentStructure = node1.predicateArgumentStructure,
            localContext = node1.localContext,
            extentText = node1.extentText)

          val knowledgeBaseNode2 = KnowledgeBaseNode(
            nodeId = node2.nodeId,
            propositionId = node2.propositionId,
            sentenceId = node2.sentenceId,
            predicateArgumentStructure = node2.predicateArgumentStructure,
            localContext = node2.localContext,
            extentText = node2.extentText)


          val edge:KnowledgeBaseEdge = x(1).value.logicEdge
          val logicEdge:KnowledgeBaseEdge = KnowledgeBaseEdge(node1.nodeId,node2.nodeId, edge.caseStr, edge.dependType, edge.logicType, edge.lang)

          val dataInfo:(Map[String, KnowledgeBaseNode], List[KnowledgeBaseEdge]) = acc.isDefinedAt(key) match {
            case true => acc.get(key).get
            case _ => (Map.empty[String, KnowledgeBaseNode], List.empty[KnowledgeBaseEdge])
          }

          val nodeAndEdgeInfo:(Map[String, KnowledgeBaseNode], List[KnowledgeBaseEdge]) = (dataInfo._1 ++ Map(node1.nodeId -> knowledgeBaseNode1) ++ Map(node2.nodeId -> knowledgeBaseNode2), dataInfo._2 :+ logicEdge)
          acc ++ Map(key -> nodeAndEdgeInfo)
        }
      }
    }
    val deductionResult:Map[String, DeductionResult] =
      Map(
        PREMISE.index.toString -> DeductionResult(false, List.empty[String], ""),
        CLAIM.index.toString -> DeductionResult(false, List.empty[String],"")
      )
    val asoList = neo4jDataInfo.map(x => {
      val sentenceId = x._2._1.head._2.nodeId.substring(0, x._2._1.head._2.nodeId.lastIndexOf("-"))
      val lang = x._2._1.head._2.localContext.lang
      AnalyzedSentenceObject(x._2._1, x._2._2, sentenceType, sentenceId, lang,  deductionResult)
    }).toList
    AnalyzedSentenceObjects(asoList)

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

  /**
   * This function checks if there is a result with only the specified ID
   * @param id
   * @param record
   * @return
   */
  def existALlPropositionIdEqualId(id:String, record:List[Neo4jRecordMap]):Boolean = Try{
    if(record.size > 0){
      record.foreach { map: Neo4jRecordMap =>
        if (map.value.logicNode.propositionId.equals(id)) {
          return true
        }
      }
    }
    return false
  }match {
    case Failure(e) => throw e
  }

}
