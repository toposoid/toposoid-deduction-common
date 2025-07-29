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

import com.ideal.linked.common.DeploymentConverter.conf
import com.ideal.linked.toposoid.common.{CLAIM, ToposoidUtils, TransversalState}
import com.ideal.linked.toposoid.deduction.common.FacadeForAccessNeo4J.extractExistInNeo4JResultForSentence
import com.ideal.linked.toposoid.knowledgebase.featurevector.model.{FeatureVectorIdentifier, FeatureVectorSearchResult, SingleFeatureVectorForSearch}
import com.ideal.linked.toposoid.knowledgebase.regist.model.Knowledge
import com.ideal.linked.toposoid.protocol.model.base.{KnowledgeBaseSideInfo, MatchedFeatureInfo}
import com.ideal.linked.toposoid.vectorizer.FeatureVectorizer
import com.typesafe.scalalogging.LazyLogging
import play.api.libs.json.Json

object FacadeForAccessVectorDB  extends LazyLogging{

  def getMatchedSentenceFeature(originalSentenceId: String, originalSentenceType: Int, sentence: String, lang: String, transversalState:TransversalState): List[KnowledgeBaseSideInfo] = {

    val vector = FeatureVectorizer.getSentenceVector(Knowledge(sentence, lang, "{}"), transversalState)
    val json: String = Json.toJson(SingleFeatureVectorForSearch(vector = vector.vector, num = conf.getString("TOPOSOID_SENTENCE_VECTORDB_SEARCH_NUM_MAX").toInt)).toString()
    val featureVectorSearchResultJson: String = ToposoidUtils.callComponent(json, conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_HOST"), conf.getString("TOPOSOID_SENTENCE_VECTORDB_ACCESSOR_PORT"), "search", transversalState)
    val result = Json.parse(featureVectorSearchResultJson).as[FeatureVectorSearchResult]

    //VecotrDBにClaimとして存在している場合に推論が可能になる
    val (ids, similarities) = (result.ids zip result.similarities).foldLeft((List.empty[FeatureVectorIdentifier], List.empty[Float])) {
      (acc, x) => {
        x._1.sentenceType match {
          case CLAIM.index => (acc._1 :+ x._1, acc._2 :+ x._2)
          case _ => acc
        }
      }
    }

    val filteredResult = FeatureVectorSearchResult(ids, similarities, result.statusInfo)
    filteredResult.ids.size match {
      case 0 => List.empty[KnowledgeBaseSideInfo]
      case _ => {
        //sentenceごとに最も類似度が高いものを抽出する
        val featureVectorSearchInfoList = extractExistInNeo4JResultForSentence(filteredResult, originalSentenceType, transversalState)
        featureVectorSearchInfoList.map(x => {
          KnowledgeBaseSideInfo(x.propositionId, x.sentenceId, List(MatchedFeatureInfo(featureId = x.sentenceId, similarity = x.similarity)))
        })
      }
    }
  }


}
