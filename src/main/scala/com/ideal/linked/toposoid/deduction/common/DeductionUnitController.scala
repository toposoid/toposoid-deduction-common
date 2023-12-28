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
import com.ideal.linked.toposoid.knowledgebase.model.{KnowledgeBaseEdge, KnowledgeBaseSemiGlobalNode}
import com.ideal.linked.toposoid.protocol.model.base.{CoveredPropositionResult, _}
import com.ideal.linked.toposoid.protocol.model.neo4j.{Neo4jRecords}
import com.typesafe.scalalogging.LazyLogging
import play.api.libs.json.{JsValue, Json}
import play.api.mvc._


trait DeductionUnitController extends LazyLogging {
  protected def execute: Action[JsValue]

  protected def analyzeGraphKnowledge(edge: KnowledgeBaseEdge, aso:AnalyzedSentenceObject, accParent: List[(KnowledgeBaseSideInfo, CoveredPropositionEdge)]): List[(KnowledgeBaseSideInfo, CoveredPropositionEdge)]

  private def getMergedKnowledgeBaseSideInfo(coveredPropositionResults: List[(KnowledgeBaseSideInfo, CoveredPropositionEdge)], confirmedCoveredPropositionResults:List[CoveredPropositionResult]):List[KnowledgeBaseSideInfo] = {
    val knowledgeBaseSideInfoList = coveredPropositionResults.map(_._1)
    val confirmedKnowledgeBaseSideInfoList:List[KnowledgeBaseSideInfo] = confirmedCoveredPropositionResults.map(_.knowledgeBaseSideInfoList).flatten
    knowledgeBaseSideInfoList ++ confirmedKnowledgeBaseSideInfoList
  }

  /**
   * final check
   *
   * @param targetMatchedPropositionInfoList
   * @param aso
   * @param searchResults
   * @return
   */
  private def checkFinal(aso: AnalyzedSentenceObject, deductionUnitName:String, unsettledCoveredPropositionResults:List[(KnowledgeBaseSideInfo, CoveredPropositionEdge)] ): AnalyzedSentenceObject = {

    //The targetMatchedPropositionInfoList contains duplicate propositionIds.
    //Pick up the most frequent propositionId
    val mergedKnowledgeBaseSideInfo =  getMergedKnowledgeBaseSideInfo(unsettledCoveredPropositionResults, aso.deductionResult.coveredPropositionResults)
    val updatedCoveredPropositionResults = addCoveredPropositionResults(mergedKnowledgeBaseSideInfo, aso.deductionResult, unsettledCoveredPropositionResults, aso.knowledgeBaseSemiGlobalNode, deductionUnitName)

    val updateDeductionResult: DeductionResult = new DeductionResult(
      aso.deductionResult.status,
      updatedCoveredPropositionResults,
      aso.deductionResult.havePremiseInGivenProposition
    )
    val updateAso = AnalyzedSentenceObject(aso.nodeMap, aso.edgeList, aso.knowledgeBaseSemiGlobalNode, updateDeductionResult)

    val dupFreq = mergedKnowledgeBaseSideInfo.map(_.propositionId).groupBy(identity).filter(x => x._2.size >= aso.edgeList.size)
    if (dupFreq.size == 0) return updateAso

    //被覆サイズが最小のものを選ぶ。
    val minFreqSize = dupFreq.mapValues(_.size).minBy(_._2)._2
    val propositionIdsHavingMinFreq: List[String] = mergedKnowledgeBaseSideInfo.map(_.propositionId).groupBy(identity).mapValues(_.size).filter(_._2 == minFreqSize).map(_._1).toList
    logger.debug(propositionIdsHavingMinFreq.toString())

    val coveredPropositionInfoList = mergedKnowledgeBaseSideInfo.filter(x =>  propositionIdsHavingMinFreq.contains(x.propositionId))
    //Does the chosen proposalId have a premise? T
    //he coveredPropositionInfoList contains a mixture of those that are established only by Claims and those that have Premise.
    val propositionInfoListHavingPremise: List[KnowledgeBaseSideInfo] = coveredPropositionInfoList.filter(havePremise(_))
    val propositionInfoListOnlyClaim: List[KnowledgeBaseSideInfo] = coveredPropositionInfoList.filterNot(x => propositionInfoListHavingPremise.map(y => y.propositionId).contains(x.propositionId))

    val finalPropositionInfoList: List[KnowledgeBaseSideInfo] = propositionInfoListHavingPremise.size match {
      case 0 => propositionInfoListOnlyClaim
      case _ => propositionInfoListOnlyClaim ::: checkClaimHavingPremise(propositionInfoListHavingPremise)
    }

    if (finalPropositionInfoList.size == 0) return updateAso

    val status = true
    //selectedPropositions includes trivialClaimsPropositionIds
    val updatedCoveredPropositionResults2 = updatedCoveredPropositionResults.foldLeft(List.empty[CoveredPropositionResult]){
      (acc, x) => {
        if(x.deductionUnit.equals(deductionUnitName)){
          acc :+ CoveredPropositionResult(
            deductionUnit = x.deductionUnit,
            propositionId = x.propositionId,
            sentenceId = x.sentenceId,
            coveredPropositionEdges = x.coveredPropositionEdges,
            knowledgeBaseSideInfoList = finalPropositionInfoList)
        }else{
          acc :+ x
        }
      }
    }

    val deductionResult: DeductionResult = new DeductionResult(status, updatedCoveredPropositionResults2)
    //val updateDeductionResult = aso.deductionResult.updated(aso.knowledgeBaseSemiGlobalNode.sentenceType.toString, deductionResult)
    AnalyzedSentenceObject(aso.nodeMap, aso.edgeList, aso.knowledgeBaseSemiGlobalNode, deductionResult)

  }

  private def addCoveredPropositionResults(mergedKnowledgeBaseSideInfo:List[KnowledgeBaseSideInfo] , deductionResult:DeductionResult, unsettledCoveredPropositionResults:List[(KnowledgeBaseSideInfo, CoveredPropositionEdge)], knowledgeBaseSemiGlobalNode:KnowledgeBaseSemiGlobalNode, deductionUnitName:String): List[CoveredPropositionResult] = {

    //TODO:もっと良い方法がないか見直し
    val dupFreq = mergedKnowledgeBaseSideInfo.map(_.propositionId).groupBy(identity).filter(x => x._2.size > deductionResult.coveredPropositionResults.size)
    if(dupFreq.size == 0) return deductionResult.coveredPropositionResults
    val minFreqSize = dupFreq.mapValues(_.size).minBy(_._2)._2

    val propositionIdsHavingMinFreq: List[String] = mergedKnowledgeBaseSideInfo.map(_.propositionId).groupBy(identity).mapValues(_.size).filter(_._2 == minFreqSize).map(_._1).toList
    val filteredKnowledgeBaseSideInfo = mergedKnowledgeBaseSideInfo.filter(x =>  propositionIdsHavingMinFreq.contains(x.propositionId))

    val filteredCoveredPropositionEdges:List[CoveredPropositionEdge] = unsettledCoveredPropositionResults.filter(
      x => {
        propositionIdsHavingMinFreq.contains(x._1.propositionId)
      }).map(y => {
          y._2
      })

    val coveredPropositionResult = CoveredPropositionResult(
      deductionUnit = deductionUnitName,
      propositionId = knowledgeBaseSemiGlobalNode.propositionId,
      sentenceId = knowledgeBaseSemiGlobalNode.sentenceId,
      coveredPropositionEdges = filteredCoveredPropositionEdges,
      knowledgeBaseSideInfoList = filteredKnowledgeBaseSideInfo
      )

    deductionResult.coveredPropositionResults :+ coveredPropositionResult
  }
  /**
   *
   * @param matchedPropositionInfo
   * @return
   */
  private def havePremise(matchedPropositionInfo: KnowledgeBaseSideInfo): Boolean = {
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
  private def checkClaimHavingPremise(targetMatchedPropositionInfoList: List[KnowledgeBaseSideInfo]): List[KnowledgeBaseSideInfo] = {
    //Pick up a node with the same surface layer as the Premise connected from Claim as x
    //Search for the one that has the corresponding ClaimId and has a premise
    targetMatchedPropositionInfoList.foldLeft(List.empty[KnowledgeBaseSideInfo]) {
      (acc, x) => {
        val query = "MATCH (n1:PremiseNode)-[e:LocalEdge{logicType:'-'}]->(n2:PremiseNode) WHERE n1.propositionId='%s' AND n2.propositionId='%s' RETURN n1, e, n2".format(x.propositionId, x.propositionId)
        val jsonStr = FacadeForAccessNeo4J.getCypherQueryResult(query, "x")
        val neo4jRecords: Neo4jRecords = Json.parse(jsonStr).as[Neo4jRecords]
        val resultMatchedPropositionInfoList = neo4jRecords.records.size match {
          case 0 => List.empty[KnowledgeBaseSideInfo]
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
  private def checkOnlyClaimNodes(neo4jRecords: Neo4jRecords, targetMatchedPropositionInfoList: List[KnowledgeBaseSideInfo]): List[KnowledgeBaseSideInfo] = {

    val claimMatchedPropositionInfo: List[KnowledgeBaseSideInfo] = neo4jRecords.records.foldLeft(List.empty[KnowledgeBaseSideInfo]) {
      (acc, x) => {
        val surface1: String = x(0).value.localNode.get.predicateArgumentStructure.surface
        val caseStr: String = x(1).value.localEdge.get.caseStr
        val surface2: String = x(2).value.localNode.get.predicateArgumentStructure.surface
        val query = "MATCH (n1:ClaimNode)-[e:LocalEdge]->(n2:ClaimNode) WHERE n1.surface='%s' AND e.caseName='%s' AND n2.surface='%s' RETURN n1, e, n2".format(surface1, caseStr, surface2)
        val jsonStr: String = getCypherQueryResult(query, "")
        val neo4jRecordsForClaim: Neo4jRecords = Json.parse(jsonStr).as[Neo4jRecords]
        val additionalMatchedPropositionInfo = neo4jRecordsForClaim.records.foldLeft(List.empty[KnowledgeBaseSideInfo]) {
          (acc2, x2) => {
            val propositionId = x2.head.value.localNode.get.propositionId
            val sentenceId = x2.head.value.localNode.get.sentenceId
            val matchedFeatureInfo = MatchedFeatureInfo(sentenceId, 1)
            acc2 :+ KnowledgeBaseSideInfo(propositionId, sentenceId, List(matchedFeatureInfo))
          }
        }
        acc ::: additionalMatchedPropositionInfo
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
      case _ => finalChoice ::: targetMatchedPropositionInfoList
    }

  }

  /**
   *
   * @param aso
   * @return
   */
  private def getUnsettledEdges(aso:AnalyzedSentenceObject): List[KnowledgeBaseEdge] = {
    val pairSetList = aso.deductionResult.coveredPropositionResults.foldLeft(List.empty[Set[String]]){
        (acc, x) => {
          acc ++ x.coveredPropositionEdges.foldLeft(List.empty[Set[String]]) {
            (acc, y) => {
              acc :+ Set(y.sourceNode.terminalId, y.destinationNode.terminalId)
            }
          }
        }
      }
    aso.edgeList.filterNot(x => {
      val targetLink = Set(x.sourceId, x.destinationId)
      pairSetList.contains(targetLink)
    })
  }

  /**
   *
   * @param edge
   * @param aso
   * @param deductionUnitFeatureTypes
   * @return
   */
  private def haveFeatureTypeToProcess(edge: KnowledgeBaseEdge, aso: AnalyzedSentenceObject, deductionUnitFeatureTypes:List[Int]): Boolean = {
    val sourceKnowledgeFeatureReferences = aso.nodeMap.get(edge.sourceId).get.localContext.knowledgeFeatureReferences
    val destinationKnowledgeFeatureReferences = aso.nodeMap.get(edge.destinationId).get.localContext.knowledgeFeatureReferences
    val isSourceSideOk = sourceKnowledgeFeatureReferences.size match {
      case 0 =>  true
      case _ => {
        sourceKnowledgeFeatureReferences.filter(x => deductionUnitFeatureTypes.contains(x.featureType)).size > 0
      }
    }
    val isDestinationSideOk = destinationKnowledgeFeatureReferences.size match {
      case 0 => true
      case _ => {
        destinationKnowledgeFeatureReferences.filter(x => deductionUnitFeatureTypes.contains(x.featureType)).size > 0
      }
    }
    isSourceSideOk && isDestinationSideOk
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
    val coveredPropositionResults = getUnsettledEdges(aso).foldLeft(List.empty[(KnowledgeBaseSideInfo, CoveredPropositionEdge)]) {
      (acc, x) => {
        //If the feature does not match, it cannot be evaluated and will be skipped.
        if (haveFeatureTypeToProcess(x, aso, deductionUnitFeatureTypes)) {
          analyzeGraphKnowledge(x, aso, acc)
        } else{
          acc
        }
      }
    }
    if (coveredPropositionResults.size == 0) return aso
    val result = checkFinal(aso, deductionUnitName, coveredPropositionResults)
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


