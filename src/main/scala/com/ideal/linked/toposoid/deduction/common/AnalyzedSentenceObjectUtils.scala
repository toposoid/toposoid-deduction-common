package com.ideal.linked.toposoid.deduction.common

import com.ideal.linked.toposoid.common.{CLAIM, PREMISE}
import com.ideal.linked.toposoid.protocol.model.base.AnalyzedSentenceObject

/**
 *　Utilities for AnalyzedSentenceObject
 */
object AnalyzedSentenceObjectUtils {
  def makeSentence(aso: AnalyzedSentenceObject): (Map[Int, String], String) ={
    val surfaces:Map[Int, (String, Int)] = aso.nodeMap.map(node => {
      (node._2.currentId -> (node._2.surface, aso.sentenceType))
    })

    val space = aso.nodeMap.head._2.lang match {
      case "ja_JP" => ""
      case "en_US" => " "
      case _ => " "
    }

    val sentenceMap = surfaces.toSeq.sortBy(_._1).foldLeft(Map.empty[Int, String]){
      (acc, surfaceTuple) =>{
        val premiseSentence:String = acc.get(PREMISE.index).getOrElse("")
        val claimSentence:String = acc.get(CLAIM.index).getOrElse("")
        val sentenceMap:Map[Int, String] = surfaceTuple._2._2 match {
          case PREMISE.index => Map(PREMISE.index -> (premiseSentence + space + surfaceTuple._2._1).trim)
          case CLAIM.index => Map(CLAIM.index -> (claimSentence + space + surfaceTuple._2._1).trim)
          case _ => acc
        }
        acc ++ sentenceMap
      }
    }
    val lang = aso.nodeMap.head._2.lang
    (sentenceMap ,lang)
  }
}
