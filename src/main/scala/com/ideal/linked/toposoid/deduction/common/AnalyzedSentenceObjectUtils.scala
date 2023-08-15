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
import com.ideal.linked.toposoid.protocol.model.base.AnalyzedSentenceObject

/**
 *　Utilities for AnalyzedSentenceObject
 */
case class SentenceInfo(sentence:String, lang:String, sentenceId:String, propositionId:String)

object AnalyzedSentenceObjectUtils {

  def makeSentence(aso: AnalyzedSentenceObject): Map[Int, SentenceInfo] ={
    val propositionId = aso.nodeMap.head._2.propositionId
    val sentenceId = aso.knowledgeFeatureNode.sentenceId
    val surfaces:Map[Int, (String, Int, String)] = aso.nodeMap.map(node => {
      (node._2.predicateArgumentStructure.currentId -> (node._2.predicateArgumentStructure.surface, aso.knowledgeFeatureNode.sentenceType, node._2.localContext.lang))
    })

    val space = aso.nodeMap.head._2.localContext.lang match {
      case "ja_JP" => ""
      case "en_US" => " "
      case _ => " "
    }

    val sentenceMap = surfaces.toSeq.sortBy(_._1).foldLeft(Map.empty[Int, SentenceInfo]){
      (acc, surfaceTuple) =>{
        val premiseSentence:SentenceInfo = acc.get(PREMISE.index).getOrElse(SentenceInfo("","", "", ""))
        val claimSentence:SentenceInfo = acc.get(CLAIM.index).getOrElse(SentenceInfo("", "", "", ""))
        val sentenceMap:Map[Int, SentenceInfo] = surfaceTuple._2._2 match {
          case PREMISE.index => Map(PREMISE.index -> SentenceInfo((premiseSentence.sentence + space + surfaceTuple._2._1).trim, surfaceTuple._2._3, sentenceId, propositionId))
          case CLAIM.index => Map(CLAIM.index -> SentenceInfo((claimSentence.sentence + space + surfaceTuple._2._1).trim, surfaceTuple._2._3, sentenceId, propositionId))
          case _ => acc
        }
        acc ++ sentenceMap
      }
    }
    sentenceMap
  }
}
