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

import com.ideal.linked.toposoid.common.{CLAIM, PREMISE}
import com.ideal.linked.toposoid.deduction.common.FacadeForAccessNeo4J.getCypherQueryResult
import com.ideal.linked.toposoid.deduction.common.FacadeForAccessVectorDB.getMatchedSentenceFeature
import com.ideal.linked.toposoid.protocol.model.base._
import com.ideal.linked.toposoid.protocol.model.neo4j.Neo4jRecords
import com.typesafe.scalalogging.LazyLogging
import play.api.libs.json.{JsValue, Json}
import play.api.mvc._

case class FeatureVectorSearchInfo(propositionId:String, sentenceId:String, sentenceType:Int, lang:String, similarity:Float)
//case class SentenceId2FeatureVectorSearchResult(originalSentenceId:String, status:Boolean, featureVectorSearchInfo:FeatureVectorSearchInfo)

trait DeductionUnitControllerForSemiGlobal extends LazyLogging {
  protected def execute: Action[JsValue]

  protected def analyzeGraphKnowledgeForSemiGlobal(aso: AnalyzedSentenceObject):List[KnowledgeBaseSideInfo]
  /**
   * final check
   *
   * @param targetMatchedPropositionInfoList
   * @param aso
   * @param searchResults
   * @return
   */
  private def checkFinal(aso: AnalyzedSentenceObject, deductionUnitName:String, knowledgeBaseSideInfoList:List[KnowledgeBaseSideInfo] ): AnalyzedSentenceObject = {

    //AnalysisSentenceObjectは必ず文章は一つ
    //TODO:knowledgeBaseSideInfoListは、ある閾値を超えてるのであればいくつか候補が欲しい。ただし類似度が高い順に返して欲しい。

    //TODO:ここはFilterではうまく行かない。Premiseを立証するClaimの情報が追加されないといけない！！！
    val selectedKnowledgeBaseSideInfo:List[KnowledgeBaseSideInfo] = knowledgeBaseSideInfoList.foldLeft(List.empty[List[KnowledgeBaseSideInfo]]){
      (acc, x) => {
        if (havePremise(x)) {
          acc :+ checkClaimHavingPremise(x)
        } else {
          acc :+ List(x)
        }
      }
    }.head

    if(selectedKnowledgeBaseSideInfo.size == 0) return aso
    //TODO:updated CoveredPropositionResults Impl　基本ALL or Nothing 曖昧性を考慮して正しいのであればその命題はすべて正しい。
    // このDeductionUnitにおいて部分的にマッチしているということはない。

    val coveredPropositionEdges: List[CoveredPropositionEdge] = aso.edgeList.map(x => {
      val sourceSurface = aso.nodeMap.get(x.sourceId).get.predicateArgumentStructure.surface
      val destinationSurface = aso.nodeMap.get(x.destinationId).get.predicateArgumentStructure.surface
      val sourceNode = CoveredPropositionNode(terminalId = x.sourceId, terminalSurface = sourceSurface, terminalUrl = "")
      val destinationNode = CoveredPropositionNode(terminalId = x.destinationId, terminalSurface = destinationSurface, terminalUrl = "")
      CoveredPropositionEdge(sourceNode = sourceNode, destinationNode = destinationNode)
    })

    val updatedCoveredPropositionResults = aso.deductionResult.coveredPropositionResults :+
      CoveredPropositionResult(
      deductionUnit = deductionUnitName,
      propositionId = aso.knowledgeBaseSemiGlobalNode.propositionId,
      sentenceId = aso.knowledgeBaseSemiGlobalNode.sentenceId,
      coveredPropositionEdges = coveredPropositionEdges,
      knowledgeBaseSideInfoList = selectedKnowledgeBaseSideInfo)


    val status = true
    val deductionResult: DeductionResult = new DeductionResult(status, updatedCoveredPropositionResults)
    AnalyzedSentenceObject(aso.nodeMap, aso.edgeList, aso.knowledgeBaseSemiGlobalNode, deductionResult)

  }

  /**
   *
   * @param knowledgeBaseSideInfo
   * @return
   */
  private def havePremise(knowledgeBaseSideInfo: KnowledgeBaseSideInfo): Boolean = {
    val query = "MATCH (n:SemiGlobalPremiseNode)-[*]-(m:SemiGlobalClaimNode) WHERE m.propositionId ='%s' AND m.sentenceId ='%s'  RETURN (n)".format(knowledgeBaseSideInfo.propositionId, knowledgeBaseSideInfo.sentenceId)
    val jsonStr: String = getCypherQueryResult(query, "n")
    val neo4jRecords: Neo4jRecords = Json.parse(jsonStr).as[Neo4jRecords]
    neo4jRecords.records.size match {
      case 0 => false
      case _ => true
    }
  }

  /**
   *
   * @param targetMatchedPropositionInfoList
   * @return
   */
  private def checkClaimHavingPremise(knowledgeBaseSideInfo: KnowledgeBaseSideInfo): List[KnowledgeBaseSideInfo] = {
    //Pick up a node with the same surface layer as the Premise connected from Claim as x
    //Search for the one that has the corresponding ClaimId and has a premise

    val query = "MATCH (n:SemiGlobalPremiseNode) WHERE n.propositionId='%s' RETURN n".format(knowledgeBaseSideInfo.propositionId, knowledgeBaseSideInfo.propositionId)
    val jsonStr = FacadeForAccessNeo4J.getCypherQueryResult(query, "x")
    val neo4jRecords: Neo4jRecords = Json.parse(jsonStr).as[Neo4jRecords]
    neo4jRecords.records.size match {
      case 0 => List.empty[KnowledgeBaseSideInfo]
      case _ => checkOnlyClaimNodes(neo4jRecords, knowledgeBaseSideInfo)
    }
  }


  /**
   *
   * @param neo4jRecords
   * @param targetMatchedPropositionInfoList
   * @return
   */
  private def checkOnlyClaimNodes(neo4jRecords: Neo4jRecords, knowledgeBaseSideInfo: KnowledgeBaseSideInfo): List[KnowledgeBaseSideInfo] = {

    val claimMatchedPropositionInfo = neo4jRecords.records.foldLeft(List.empty[KnowledgeBaseSideInfo]){
      (acc, x) => {
        //得られたすべてのPremiseについてClaimが存在するかをチェック　
        val originalSentenceId = x.head.value.semiGlobalNode.get.sentenceId
        val originalSentenceType = x.head.value.semiGlobalNode.get.sentenceType
        val sentence = x.head.value.semiGlobalNode.get.sentence
        val lang = x.head.value.semiGlobalNode.get.localContextForFeature.lang

        acc ::: getMatchedSentenceFeature(originalSentenceId, originalSentenceType, sentence, lang)
      }
    }

    //Checkpoint
    //・Are there all claims corresponding to premise?
    //・Does the obtained result have more propositionIds than the number of neo4jRecords records?得られてた結果でneo4jRecordsのレコード数と同数以上のpropositionIdを持つものが存在するかどうか？
    //・Multiple claims can guarantee one Premise, so it is not necessarily =, but there must be more Claims than the number of Premises.
    if (claimMatchedPropositionInfo.size < neo4jRecords.records.size) return List.empty[KnowledgeBaseSideInfo]

    //val candidates: List[MatchedPropositionInfo] = claimMatchedPropositionInfo.groupBy(identity).mapValues(_.size).map(_._1).toList
    val candidates: List[KnowledgeBaseSideInfo] = claimMatchedPropositionInfo.distinct
    //candidatesは、propositionId上の重複はない。
    if (candidates.size == 0) return List.empty[KnowledgeBaseSideInfo]
    //ensure there are no Premise. only claim!
    val finalChoice: List[KnowledgeBaseSideInfo] = candidates.filterNot(x => this.havePremise(x))
    finalChoice.size match {
      case 0 => List.empty[KnowledgeBaseSideInfo]
      case _ => finalChoice :+ knowledgeBaseSideInfo //finalChoice:premiseを説明するClaim knowledgeBaseSideInfo: premiseに接続しているClaim
    }

  }

  /**
   *
   * @param edge
   * @param aso
   * @param deductionUnitFeatureTypes
   * @return
   */
  private def haveFeatureTypeToProcess(aso: AnalyzedSentenceObject, deductionUnitFeatureTypes:List[Int]): Boolean = {
    val knowledgeFeatureReferences = aso.knowledgeBaseSemiGlobalNode.localContextForFeature.knowledgeFeatureReferences
    knowledgeFeatureReferences.size match {
      case 0 =>  true
      case _ => {
        knowledgeFeatureReferences.filter((x => deductionUnitFeatureTypes.contains(x.featureType))).size > 0
      }
    }
  }

  /**
   * This function analyzes whether the entered text exactly matches.
   *
   * @param aso
   * @param asos
   * @return
   */
  def analyze(aso: AnalyzedSentenceObject, asos: List[AnalyzedSentenceObject], deductionUnitName:String, deductionUnitFeatureTypes:List[Int]): AnalyzedSentenceObject = {
    //Excluding those for which the existence of links has already been confirmed in edgeList

    //TODO:ASOの置き換えはしない方向で大丈夫か確認する。
    if(!haveFeatureTypeToProcess(aso, deductionUnitFeatureTypes)) return aso
    val knowledgeBaseSideInfoList:List[KnowledgeBaseSideInfo] = analyzeGraphKnowledgeForSemiGlobal(aso)
    if (knowledgeBaseSideInfoList.size == 0) return aso

    val result = checkFinal(aso, deductionUnitName, knowledgeBaseSideInfoList)
    if(!result.deductionResult.status) return result
    //This process requires that the Premise has already finished in calculating the DeductionResult
    if (aso.knowledgeBaseSemiGlobalNode.sentenceType == CLAIM.index) {

      //val premiseDeductionResults: List[DeductionResult] = asos.map(x => x.deductionResultMap.get(PREMISE.index.toString).get)
      val premiseDeductionResults: List[DeductionResult] = asos.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType == PREMISE.index).map(y => y.deductionResult)
      //If there is no deduction result that makes premise true, return the process.
      if (premiseDeductionResults.filter(_.status).size == 0) return result
      asos.filter(x => x.knowledgeBaseSemiGlobalNode.sentenceType == PREMISE.index).size match {
        case 0 => result
        case _ => {
          //val premiseDeductionResults: List[DeductionResult] = asos.map(x => x.deductionResultMap.get(PREMISE.index.toString).get)
          val knowledgeBaseSideInfoList: List[KnowledgeBaseSideInfo] = premiseDeductionResults.map(_.coveredPropositionResults.map(_.knowledgeBaseSideInfoList).flatten).flatten
          val premisePropositionIds: Set[String] = knowledgeBaseSideInfoList.map(_.propositionId).toSet
          //val claimPropositionIds: Set[String] = result.deductionResultMap.get(CLAIM.index.toString).get.matchedPropositionInfoList.map(_.propositionId).toSet[String]
          //Depending on the conditions, the result is claim information.
          val claimPropositionIds:Set[String] = result.deductionResult.coveredPropositionResults.map(_.knowledgeBaseSideInfoList.map(_.propositionId)).flatten.toSet[String]
          //There must be at least one Claim that corresponds to at least one Premise proposition.
          (premisePropositionIds & claimPropositionIds).size - premisePropositionIds.size match {
            case 0 => {
              //val originalDeductionResult: DeductionResult = result.deductionResultMap.get(CLAIM.index.toString).get
              val originalDeductionResult: DeductionResult = result.deductionResult
              val updateDeductionResult: DeductionResult = DeductionResult(
                status = originalDeductionResult.status,
                coveredPropositionResults = originalDeductionResult.coveredPropositionResults,
                havePremiseInGivenProposition = true
              )
              AnalyzedSentenceObject(
                nodeMap = result.nodeMap,
                edgeList = result.edgeList,
                knowledgeBaseSemiGlobalNode = result.knowledgeBaseSemiGlobalNode,
                deductionResult = updateDeductionResult
              )
            }
            case _ => result
          }
        }
      }
    } else {
      result
    }
  }

}


