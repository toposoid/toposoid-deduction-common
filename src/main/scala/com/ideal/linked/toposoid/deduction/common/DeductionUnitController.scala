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
import com.ideal.linked.toposoid.knowledgebase.model.{KnowledgeBaseEdge, KnowledgeBaseNode, KnowledgeBaseSemiGlobalNode}
import com.ideal.linked.toposoid.protocol.model.base.{CoveredPropositionResult, _}
import com.ideal.linked.toposoid.protocol.model.neo4j.{Neo4jRecordMap, Neo4jRecords}
import com.typesafe.scalalogging.LazyLogging
import play.api.libs.json.{JsValue, Json}
import play.api.mvc._


trait DeductionUnitController extends LazyLogging {
  protected def execute: Action[JsValue]

  protected def analyzeGraphKnowledge(edge: KnowledgeBaseEdge, nodeMap: Map[String, KnowledgeBaseNode], sentenceType: Int, accParent: (List[List[Neo4jRecordMap]], List[MatchedPropositionInfo], List[CoveredPropositionEdge])): (List[List[Neo4jRecordMap]], List[MatchedPropositionInfo], List[CoveredPropositionEdge])

  /**
   * final check
   *
   * @param targetMatchedPropositionInfoList
   * @param aso
   * @param searchResults
   * @return
   */
  private def checkFinal(targetMatchedPropositionInfoList: List[MatchedPropositionInfo], aso: AnalyzedSentenceObject, searchResults: List[List[Neo4jRecordMap]], deductionUnitName:String, coveredPropositionEdgeList:List[CoveredPropositionEdge] ): AnalyzedSentenceObject = {

    val updatedCoveredPropositionResult = updateCoveredPropositionResult(aso.deductionResult, coveredPropositionEdgeList, aso.knowledgeBaseSemiGlobalNode, deductionUnitName)
    val updateDeductionResult: DeductionResult = new DeductionResult(
      aso.deductionResult.status,
      aso.deductionResult.matchedPropositionInfoList,
      aso.deductionResult.deductionUnit,
      updatedCoveredPropositionResult,
      aso.deductionResult.havePremiseInGivenProposition
    )
    val updateAso = AnalyzedSentenceObject(aso.nodeMap, aso.edgeList, aso.knowledgeBaseSemiGlobalNode, updateDeductionResult)

    //The targetMatchedPropositionInfoList contains duplicate propositionIds.
    if (targetMatchedPropositionInfoList.size < aso.edgeList.size) return updateAso
    //Pick up the most frequent propositionId
    val dupFreq = targetMatchedPropositionInfoList.groupBy(identity).filter(x => x._2.size >= aso.edgeList.size)
    if (dupFreq.size == 0) return updateAso

    val minFreqSize = dupFreq.mapValues(_.size).minBy(_._2)._2
    val propositionIdsHavingMinFreq: List[MatchedPropositionInfo] = targetMatchedPropositionInfoList.groupBy(identity).mapValues(_.size).filter(_._2 == minFreqSize).map(_._1).toList
    logger.debug(propositionIdsHavingMinFreq.toString())

    val coveredPropositionInfoList = propositionIdsHavingMinFreq
    //Does the chosen proposalId have a premise? T
    //he coveredPropositionInfoList contains a mixture of those that are established only by Claims and those that have Premise.
    val propositionInfoListHavingPremise: List[MatchedPropositionInfo] = coveredPropositionInfoList.filter(havePremise(_))
    val propositionInfoListOnlyClaim: List[MatchedPropositionInfo] = coveredPropositionInfoList.filterNot(x => propositionInfoListHavingPremise.map(y => y.propositionId).contains(x.propositionId))

    val finalPropositionInfoList: List[MatchedPropositionInfo] = propositionInfoListHavingPremise.size match {
      case 0 => propositionInfoListOnlyClaim
      case _ => propositionInfoListOnlyClaim ::: checkClaimHavingPremise(propositionInfoListHavingPremise)
    }

    if (finalPropositionInfoList.size == 0) return updateAso

    val status = true
    //selectedPropositions includes trivialClaimsPropositionIds
    val deductionResult: DeductionResult = new DeductionResult(status, finalPropositionInfoList, deductionUnitName, updatedCoveredPropositionResult)
    //val updateDeductionResult = aso.deductionResult.updated(aso.knowledgeBaseSemiGlobalNode.sentenceType.toString, deductionResult)
    AnalyzedSentenceObject(aso.nodeMap, aso.edgeList, aso.knowledgeBaseSemiGlobalNode, deductionResult)

  }

  private def updateCoveredPropositionResult(deductionResult:DeductionResult, coveredPropositionEdgeList:List[CoveredPropositionEdge], knowledgeBaseSemiGlobalNode:KnowledgeBaseSemiGlobalNode, deductionUnitName:String): CoveredPropositionResult = {
    //もし該当のDeductionUnitが多く被覆していたらその情報を置き換える。
    if(deductionResult.coveredPropositionResult.coveredPropositionEdges.size >= coveredPropositionEdgeList.size) {
      deductionResult.coveredPropositionResult
    }else{
      CoveredPropositionResult(deductionUnit = deductionUnitName, propositionId = knowledgeBaseSemiGlobalNode.propositionId, sentenceId = knowledgeBaseSemiGlobalNode.propositionId, coveredPropositionEdges = coveredPropositionEdgeList)
    }
  }
  /**
   *
   * @param matchedPropositionInfo
   * @return
   */
  private def havePremise(matchedPropositionInfo: MatchedPropositionInfo): Boolean = {
    val query = "MATCH (n:PremiseNode)-[*]-(m:ClaimNode) WHERE m.propositionId ='%s'  RETURN (n)".format(matchedPropositionInfo.propositionId)
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
  private def checkClaimHavingPremise(targetMatchedPropositionInfoList: List[MatchedPropositionInfo]): List[MatchedPropositionInfo] = {
    //Pick up a node with the same surface layer as the Premise connected from Claim as x
    //Search for the one that has the corresponding ClaimId and has a premise
    targetMatchedPropositionInfoList.foldLeft(List.empty[MatchedPropositionInfo]) {
      (acc, x) => {
        val query = "MATCH (n1:PremiseNode)-[e:PremiseEdge]->(n2:PremiseNode) WHERE n1.propositionId='%s' AND n2.propositionId='%s' RETURN n1, e, n2".format(x.propositionId, x.propositionId)
        val jsonStr = FacadeForAccessNeo4J.getCypherQueryResult(query, "x")
        val neo4jRecords: Neo4jRecords = Json.parse(jsonStr).as[Neo4jRecords]
        val resultMatchedPropositionInfoList = neo4jRecords.records.size match {
          case 0 => List.empty[MatchedPropositionInfo]
          case _ => checkOnlyClaimNodes(neo4jRecords, targetMatchedPropositionInfoList)
        }
        acc ::: resultMatchedPropositionInfoList
      }
    }
  }

  /**
   *
   * @param neo4jRecords
   * @param targetMatchedPropositionInfoList
   * @return
   */
  private def checkOnlyClaimNodes(neo4jRecords: Neo4jRecords, targetMatchedPropositionInfoList: List[MatchedPropositionInfo]): List[MatchedPropositionInfo] = {

    val claimMatchedPropositionInfo: List[MatchedPropositionInfo] = neo4jRecords.records.foldLeft(List.empty[MatchedPropositionInfo]) {
      (acc, x) => {
        val surface1: String = x(0).value.logicNode.predicateArgumentStructure.surface
        val caseStr: String = x(1).value.logicEdge.caseStr
        val surface2: String = x(2).value.logicNode.predicateArgumentStructure.surface
        val query = "MATCH (n1:ClaimNode)-[e:ClaimEdge]->(n2:ClaimNode) WHERE n1.surface='%s' AND e.caseName='%s' AND n2.surface='%s' RETURN n1, e, n2".format(surface1, caseStr, surface2)
        val jsonStr: String = getCypherQueryResult(query, "")
        val neo4jRecordsForClaim: Neo4jRecords = Json.parse(jsonStr).as[Neo4jRecords]
        val additionalMatchedPropositionInfo = neo4jRecordsForClaim.records.foldLeft(List.empty[MatchedPropositionInfo]) {
          (acc2, x2) => {
            val propositionId = x2.head.value.logicNode.propositionId
            val sentenceId = x2.head.value.logicNode.sentenceId
            val matchedFeatureInfo = MatchedFeatureInfo(sentenceId, 1)
            acc2 :+ MatchedPropositionInfo(propositionId, List(matchedFeatureInfo))
          }
        }
        acc ::: additionalMatchedPropositionInfo
      }
    }
    //Checkpoint
    //・Are there all claims corresponding to premise?
    //・Does the obtained result have more propositionIds than the number of neo4jRecords records?得られてた結果でneo4jRecordsのレコード数と同数以上のpropositionIdを持つものが存在するかどうか？
    //・Multiple claims can guarantee one Premise, so it is not necessarily =, but there must be more Claims than the number of Premises.
    if (claimMatchedPropositionInfo.size < neo4jRecords.records.size) return List.empty[MatchedPropositionInfo]

    //val candidates: List[MatchedPropositionInfo] = claimMatchedPropositionInfo.groupBy(identity).mapValues(_.size).map(_._1).toList
    val candidates: List[MatchedPropositionInfo] = claimMatchedPropositionInfo.distinct
    //candidatesは、propositionId上の重複はない。
    if (candidates.size == 0) return List.empty[MatchedPropositionInfo]
    //ensure there are no Premise. only claim!
    val finalChoice: List[MatchedPropositionInfo] = candidates.filterNot(x => this.havePremise(x))
    finalChoice.size match {
      case 0 => List.empty[MatchedPropositionInfo]
      case _ => finalChoice ::: targetMatchedPropositionInfoList
    }

  }

  /**
   * This function analyzes whether the entered text exactly matches.
   *
   * @param aso
   * @param asos
   * @return
   */
  def analyze(aso: AnalyzedSentenceObject, asos: List[AnalyzedSentenceObject], deductionUnitName:String): AnalyzedSentenceObject = {

    val (searchResults, propositionIdInfoList, coveredPropositionEdgeList) = aso.edgeList.foldLeft((List.empty[List[Neo4jRecordMap]], List.empty[MatchedPropositionInfo], List.empty[CoveredPropositionEdge])) {
      (acc, x) => analyzeGraphKnowledge(x, aso.nodeMap, aso.knowledgeBaseSemiGlobalNode.sentenceType, acc)
    }
    if (propositionIdInfoList.size == 0) return aso
    val result = checkFinal(propositionIdInfoList, aso, searchResults, deductionUnitName, coveredPropositionEdgeList)

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
          val matchedPropositionInfoList: List[MatchedPropositionInfo] = premiseDeductionResults.map(_.matchedPropositionInfoList).flatten
          val premisePropositionIds: Set[String] = matchedPropositionInfoList.map(_.propositionId).toSet
          //val claimPropositionIds: Set[String] = result.deductionResultMap.get(CLAIM.index.toString).get.matchedPropositionInfoList.map(_.propositionId).toSet[String]
          //Depending on the conditions, the result is claim information.
          val claimPropositionIds:Set[String] = result.deductionResult.matchedPropositionInfoList.map(_.propositionId).toSet[String]
          //There must be at least one Claim that corresponds to at least one Premise proposition.
          (premisePropositionIds & claimPropositionIds).size - premisePropositionIds.size match {
            case 0 => {
              //val originalDeductionResult: DeductionResult = result.deductionResultMap.get(CLAIM.index.toString).get
              val originalDeductionResult: DeductionResult = result.deductionResult
              val updateDeductionResult: DeductionResult = DeductionResult(
                status = originalDeductionResult.status,
                matchedPropositionInfoList = originalDeductionResult.matchedPropositionInfoList,
                deductionUnit = originalDeductionResult.deductionUnit,
                coveredPropositionResult = originalDeductionResult.coveredPropositionResult,
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
  /*
  private def getPropositionIdsInDeductionPremiseResult(asos:List[AnalyzedSentenceObject]):List[String] ={
    //もし前提がある命題であれば、その演繹結果を取得
    val premisePropositions: List[AnalyzedSentenceObject] = asos.filter(x => x.knowledgeFeatureNode.sentenceType == PREMISE.index).size match {
      case 0 => List.empty[AnalyzedSentenceObject]
      case _ => asos.filter(x => x.knowledgeFeatureNode.sentenceType == PREMISE.index)
    }
    val premiseDeductionResult: List[DeductionResult] = premisePropositions.filter(_.deductionResultMap.get(PREMISE.index.toString).get.status).size match {
      case 0 => List.empty[DeductionResult]
      case _ => premisePropositions.filter(_.deductionResultMap.get(PREMISE.index.toString).get.status).map(_.deductionResultMap.get(PREMISE.index.toString).get)
    }
    premiseDeductionResult.map(_.matchedPropositionInfoList.map(_.propositionId)).flatten
  }
  */
}


