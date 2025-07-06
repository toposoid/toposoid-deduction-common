/*
 * Copyright (C) 2025  Linked Ideal LLC.[https://linked-ideal.com/]
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.ideal.linked.toposoid.deduction.common

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpMethods, HttpRequest}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import play.api.libs.json.Json
import com.ideal.linked.common.DeploymentConverter.conf
import com.ideal.linked.toposoid.common.{CLAIM, LOCAL, PREDICATE_ARGUMENT, PREMISE, SEMIGLOBAL, SENTENCE, TRANSVERSAL_STATE, ToposoidUtils, TransversalState}
import com.ideal.linked.toposoid.deduction.common.AnalyzedSentenceObjectUtils.makeSentence
import com.ideal.linked.toposoid.knowledgebase.featurevector.model.FeatureVectorSearchResult
import com.ideal.linked.toposoid.knowledgebase.model.{KnowledgeBaseEdge, KnowledgeBaseNode, KnowledgeBaseSemiGlobalNode, KnowledgeFeatureReference, LocalContextForFeature}
import com.ideal.linked.toposoid.protocol.model.base.{AnalyzedSentenceObject, AnalyzedSentenceObjects, CoveredPropositionResult, DeductionResult}
import com.ideal.linked.toposoid.protocol.model.neo4j.Neo4jRecords
import com.typesafe.scalalogging.LazyLogging

import scala.util.{Failure, Success, Try}

/**
 * Common functions used for microservices using Neo4J
 */
object FacadeForAccessNeo4J extends LazyLogging{

  def getCypherQueryResult(query:String, target:String, transversalState:TransversalState): String = Try{
    val retryNum =  conf.getInt("retryCallMicroserviceNum") -1
    for (i <- 0 to retryNum) {
      val result:String  = this.getCypherQueryResultImpl(query, target, transversalState)
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
   * @param sentenceId
   * @param sentenceType
   * @return
   */
  def getAnalyzedSentenceObjectBySentenceId(propositionId:String, sentenceId:String, sentenceType:Int, lang:String, transversalState:TransversalState):AnalyzedSentenceObject = Try {
    //Neo4JにClaimとして存在している場合に推論が可能になる
    val nodeType: String = ToposoidUtils.getNodeType(CLAIM.index, LOCAL.index, PREDICATE_ARGUMENT.index)
    val query = "MATCH (n1:%s)-[e]->(n2:%s) WHERE n1.sentenceId='%s' AND n2.sentenceId='%s' RETURN n1, e, n2".format(nodeType, nodeType, sentenceId, sentenceId)
    val jsonStr: String = getCypherQueryResult(query, "", transversalState)
    //If there is even one that does not match, it is useless to search further
    val neo4jRecords: Neo4jRecords = Json.parse(jsonStr).as[Neo4jRecords]
    //AnalyzedSentenceObjectは、与えられた命題のsentenceTypeで作成する。
    getNeo4JData2AnalyzedSentenceObject(propositionId, sentenceId, sentenceType, lang, neo4jRecords)
  } match {
    case Success(s) => s
    case Failure(e) => throw e
  }

  private def getNeo4JData2AnalyzedSentenceObject(propositionId:String, sentenceId:String, sentenceType:Int, lang:String, neo4jRecords:Neo4jRecords):AnalyzedSentenceObject = {

    val neo4jDataInfo = neo4jRecords.records.foldLeft((Map.empty[String, KnowledgeBaseNode], List.empty[KnowledgeBaseEdge])) {
      (acc, x) => {
        val node1: KnowledgeBaseNode = x(0).value.localNode.get
        val node2: KnowledgeBaseNode = x(2).value.localNode.get

        val knowledgeBaseNode1 = KnowledgeBaseNode(
          nodeId = node1.nodeId,
          propositionId = node1.propositionId,
          sentenceId = node1.sentenceId,
          predicateArgumentStructure = node1.predicateArgumentStructure,
          localContext = node1.localContext)

        val knowledgeBaseNode2 = KnowledgeBaseNode(
          nodeId = node2.nodeId,
          propositionId = node2.propositionId,
          sentenceId = node2.sentenceId,
          predicateArgumentStructure = node2.predicateArgumentStructure,
          localContext = node2.localContext)

        val edge: KnowledgeBaseEdge = x(1).value.localEdge.get
        val logicEdge: KnowledgeBaseEdge = KnowledgeBaseEdge(node1.nodeId, node2.nodeId, edge.caseStr, edge.dependType, edge.parallelType, edge.hasInclusion, edge.logicType)
        val nodeMap:Map[String, KnowledgeBaseNode] = acc._1 ++ Map(node1.nodeId -> knowledgeBaseNode1) ++ Map(node2.nodeId -> knowledgeBaseNode2)
        val edgeList:List[KnowledgeBaseEdge] = acc._2 :+ logicEdge
        (nodeMap, edgeList)
      }
    }
    /*
    val coveredPropositionResult: CoveredPropositionResult = CoveredPropositionResult(
      "",
      propositionId,
      sentenceId,
      List.empty[CoveredPropositionEdge]
    )
     */
    val deductionResult: DeductionResult = DeductionResult (false, coveredPropositionResults = List.empty[CoveredPropositionResult])

    val localContextForFeature: LocalContextForFeature = LocalContextForFeature(lang, List.empty[KnowledgeFeatureReference])
    val tmpKnowledgeBaseSemiGlobalNode: KnowledgeBaseSemiGlobalNode = KnowledgeBaseSemiGlobalNode(
      sentenceId,
      propositionId,
      sentenceId,
      "",
      sentenceType,
      localContextForFeature
    )

    AnalyzedSentenceObject(nodeMap = neo4jDataInfo._1 ,
      edgeList = neo4jDataInfo._2,
      knowledgeBaseSemiGlobalNode = tmpKnowledgeBaseSemiGlobalNode,
      deductionResult = deductionResult)
  }

  /**
   *
   * @param propositionId
   * @param sentenceType
   * @return
   */
  def neo4JData2AnalyzedSentenceObjectByPropositionId(propositionId:String, sentenceType:Int, transversalState:TransversalState):AnalyzedSentenceObjects = Try{
    val nodeType:String = ToposoidUtils.getNodeType(sentenceType, LOCAL.index, PREDICATE_ARGUMENT.index)
    val query = "MATCH (n1:%s)-[e]->(n2:%s) WHERE n1.propositionId='%s' AND n2.propositionId='%s' RETURN n1, e, n2".format(nodeType, nodeType, propositionId, propositionId)
    val jsonStr:String = getCypherQueryResult(query, "", transversalState)
    //If there is even one that does not match, it is useless to search further
    val neo4jRecords:Neo4jRecords = Json.parse(jsonStr).as[Neo4jRecords]

    val neo4jDataInfo = neo4jRecords.records.foldLeft(Map.empty[String, (Map[String, KnowledgeBaseNode], List[KnowledgeBaseEdge])]){
      (acc, x) =>{
        val node1:KnowledgeBaseNode = x(0).value.localNode.get
        val node2:KnowledgeBaseNode = x(2).value.localNode.get

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
            localContext = node1.localContext)

          val knowledgeBaseNode2 = KnowledgeBaseNode(
            nodeId = node2.nodeId,
            propositionId = node2.propositionId,
            sentenceId = node2.sentenceId,
            predicateArgumentStructure = node2.predicateArgumentStructure,
            localContext = node2.localContext)


          val edge:KnowledgeBaseEdge = x(1).value.localEdge.get
          val logicEdge:KnowledgeBaseEdge = KnowledgeBaseEdge(node1.nodeId,node2.nodeId, edge.caseStr, edge.dependType, edge.parallelType, edge.hasInclusion, edge.logicType)

          val dataInfo:(Map[String, KnowledgeBaseNode], List[KnowledgeBaseEdge]) = acc.isDefinedAt(key) match {
            case true => acc.get(key).get
            case _ => (Map.empty[String, KnowledgeBaseNode], List.empty[KnowledgeBaseEdge])
          }

          val nodeAndEdgeInfo:(Map[String, KnowledgeBaseNode], List[KnowledgeBaseEdge]) = (dataInfo._1 ++ Map(node1.nodeId -> knowledgeBaseNode1) ++ Map(node2.nodeId -> knowledgeBaseNode2), dataInfo._2 :+ logicEdge)
          acc ++ Map(key -> nodeAndEdgeInfo)
        }
      }
    }

    val asoList = neo4jDataInfo.map(x => {
      val sentenceId = x._2._1.head._2.nodeId.substring(0, x._2._1.head._2.nodeId.lastIndexOf("-"))
      val lang = x._2._1.head._2.localContext.lang
      val localContextForFeature: LocalContextForFeature = LocalContextForFeature(lang, List.empty[KnowledgeFeatureReference])
      val tmpKnowledgeFeatureNode: KnowledgeBaseSemiGlobalNode = KnowledgeBaseSemiGlobalNode(
        sentenceId,
        propositionId,
        sentenceId,
        "",
        sentenceType,
        localContextForFeature
      )

      val deductionResult: DeductionResult = DeductionResult(false, List.empty[CoveredPropositionResult])

      val aso = AnalyzedSentenceObject(x._2._1, x._2._2, tmpKnowledgeFeatureNode,  deductionResult)
      val sentenceMap = makeSentence(aso)
      val knowledgeBaseSemiGlobalNode: KnowledgeBaseSemiGlobalNode = KnowledgeBaseSemiGlobalNode(
        sentenceId,
        propositionId,
        sentenceId,
        sentenceMap.get(sentenceType).get.sentence,
        sentenceType,
        localContextForFeature
      )
      AnalyzedSentenceObject(x._2._1, x._2._2, knowledgeBaseSemiGlobalNode,  deductionResult)
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
  private def getCypherQueryResultImpl(query:String, target:String, transversalState:TransversalState): String = {

    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher
    implicit val jsonStreamingSupport: JsonEntityStreamingSupport = EntityStreamingSupport.json()
    val input = """{ "query":"%s", "target":"%s" }""".format(query, target)
    val entity = HttpEntity(ContentTypes.`application/json`, input)
    val url = "http://" + conf.getString("TOPOSOID_GRAPHDB_WEB_HOST") + ":" + conf.getString("TOPOSOID_GRAPHDB_WEB_PORT") + "/getQueryFormattedResult"
    val request = HttpRequest(uri = url, method = HttpMethods.POST, entity = entity)
                    .withHeaders(RawHeader(TRANSVERSAL_STATE.str, Json.toJson(transversalState).toString()))
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
        logger.debug(s"Success: $js")
      case Failure(e) =>
        //println(s"Failure: $e")
        logger.error(s"Failure: $e")
    }
    while(!result.isCompleted){
      Thread.sleep(20)
    }
    queryResultJson
  }

  def extractExistInNeo4JResultForSentence(featureVectorSearchResult: FeatureVectorSearchResult, originalSentenceType: Int, transversalState:TransversalState): List[FeatureVectorSearchInfo] = {

    (featureVectorSearchResult.ids zip featureVectorSearchResult.similarities).foldLeft(List.empty[FeatureVectorSearchInfo]) {
      (acc, x) => {
        val idInfo = x._1
        val propositionId = idInfo.superiorId
        val lang = idInfo.lang
        val featureId = idInfo.featureId
        val similarity = x._2
        val nodeType: String = ToposoidUtils.getNodeType(idInfo.sentenceType, SEMIGLOBAL.index, SENTENCE.index)
        //Check whether featureVectorSearchResult information exists in Neo4J
        val query = "MATCH (n:%s) WHERE n.propositionId='%s' AND n.sentenceId='%s' RETURN n".format(nodeType, propositionId, featureId)
        val jsonStr: String = getCypherQueryResult(query, "", transversalState)
        val neo4jRecords: Neo4jRecords = Json.parse(jsonStr).as[Neo4jRecords]
        neo4jRecords.records.size match {
          case 0 => acc
          case _ => {
            val idInfoOnNeo4jSide = neo4jRecords.records.head.head.value.semiGlobalNode.get
            //sentenceType returns the originalSentenceType of the argument
            acc :+ FeatureVectorSearchInfo(idInfoOnNeo4jSide.propositionId, idInfoOnNeo4jSide.sentenceId, originalSentenceType, lang, featureId, similarity)
          }
        }
      }
    }
  }

}
