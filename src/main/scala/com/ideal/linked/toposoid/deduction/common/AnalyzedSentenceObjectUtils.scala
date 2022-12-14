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
object AnalyzedSentenceObjectUtils {
  def makeSentence(aso: AnalyzedSentenceObject): Map[Int, (String, String)] ={
    val surfaces:Map[Int, (String, Int, String)] = aso.nodeMap.map(node => {
      (node._2.currentId -> (node._2.surface, aso.sentenceType, node._2.lang))
    })

    val space = aso.nodeMap.head._2.lang match {
      case "ja_JP" => ""
      case "en_US" => " "
      case _ => " "
    }

    val sentenceMap = surfaces.toSeq.sortBy(_._1).foldLeft(Map.empty[Int, (String, String)]){
      (acc, surfaceTuple) =>{
        val premiseSentence:(String, String) = acc.get(PREMISE.index).getOrElse(("", ""))
        val claimSentence:(String, String) = acc.get(CLAIM.index).getOrElse(("", ""))
        val sentenceMap:Map[Int, (String, String)] = surfaceTuple._2._2 match {
          case PREMISE.index => Map(PREMISE.index -> ((premiseSentence._1 + space + surfaceTuple._2._1).trim, premiseSentence._2))
          case CLAIM.index => Map(CLAIM.index -> ((claimSentence._1 + space + surfaceTuple._2._1).trim, claimSentence._2))
          case _ => acc
        }
        acc ++ sentenceMap
      }
    }
    sentenceMap
  }
}
