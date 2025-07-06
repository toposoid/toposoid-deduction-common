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

import com.ideal.linked.toposoid.common.{CLAIM, PREMISE}
import com.ideal.linked.toposoid.protocol.model.base.AnalyzedSentenceObject

/**
 *　Utilities for AnalyzedSentenceObject
 */
case class SentenceInfo(sentence:String, lang:String, sentenceId:String, propositionId:String)

object AnalyzedSentenceObjectUtils {

  def makeSentence(aso: AnalyzedSentenceObject): Map[Int, SentenceInfo] ={
    val propositionId = aso.nodeMap.head._2.propositionId
    val sentenceId = aso.knowledgeBaseSemiGlobalNode.sentenceId
    val surfaces:Map[Int, (String, Int, String)] = aso.nodeMap.map(node => {
      (node._2.predicateArgumentStructure.currentId -> (node._2.predicateArgumentStructure.surface, aso.knowledgeBaseSemiGlobalNode.sentenceType, node._2.localContext.lang))
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
