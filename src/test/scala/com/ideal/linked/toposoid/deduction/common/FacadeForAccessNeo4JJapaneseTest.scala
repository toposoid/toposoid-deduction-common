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

import com.ideal.linked.data.accessor.neo4j.Neo4JAccessor
import com.ideal.linked.toposoid.knowledgebase.regist.model.{Knowledge, KnowledgeSentenceSet, PropositionRelation}
import com.ideal.linked.toposoid.protocol.model.base.{AnalyzedSentenceObject, AnalyzedSentenceObjects}
import com.ideal.linked.toposoid.protocol.model.neo4j.Neo4jRecords
import com.ideal.linked.toposoid.sentence.transformer.neo4j.Sentence2Neo4jTransformer
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, DiagrammedAssertions, FlatSpec}
import play.api.libs.json.Json
import io.jvm.uuid.UUID

class FacadeForAccessNeo4JJapaneseTest extends FlatSpec with DiagrammedAssertions with BeforeAndAfter with BeforeAndAfterAll{

  override def beforeAll(): Unit = {
    Neo4JAccessor.delete()
    Sentence2Neo4jTransformer.createGraphAuto(List(UUID.random.toString), List(Knowledge("案ずるより産むが易し。", "ja_JP", "{}", false )))
  }

  override def afterAll(): Unit = {
    Neo4JAccessor.delete()
  }

  "A query for japanese knowledge" should "be handled properly" in {
    val query:String = "MATCH (n) WHERE n.lang='ja_JP' RETURN n"
    val result:String = FacadeForAccessNeo4J.getCypherQueryResult(query, "")
    val neo4jRecords: Neo4jRecords = Json.parse(result).as[Neo4jRecords]
    val sentenceMap: List[(Int, String)] = neo4jRecords.records.reverse.map(record => {
      record.filter(x => x.key.equals("n")).map(y =>
        y.value.logicNode.currentId -> y.value.logicNode.surface
      ).head
    })
    val sentence: String = sentenceMap.toSeq.sortBy(_._1).foldLeft("") { (acc, x) => acc + x._2 }
    assert(sentence.equals("案ずるより産むが易し。"))

  }

  "Neo4j data" should "be properly converted to AnalyzedSentenceObject Type" in {
    val propositionId =  UUID.random.toString
    Sentence2Neo4jTransformer.createGraphAuto(List(propositionId, propositionId), List(Knowledge("案ずるより産むが易し。", "ja_JP", "{}", false ), Knowledge("思い立ったが吉日。", "ja_JP", "{}", false )))
    val asos:AnalyzedSentenceObjects = FacadeForAccessNeo4J.neo4JData2AnalyzedSentenceObjectByPropositionId(propositionId, 1)
    assert(asos.analyzedSentenceObjects.size == 2)
    asos.analyzedSentenceObjects.foreach(aso => {
      assert(AnalyzedSentenceObjectUtils.makeSentence(aso).get(1).get.sentence == "案ずるより産むが易し。" || AnalyzedSentenceObjectUtils.makeSentence(aso).get(1).get.sentence == "思い立ったが吉日。")
    })
  }

  "havePremiseNode" should "be handled properly" in {
    val propositionId1 =  UUID.random.toString
    Sentence2Neo4jTransformer.createGraphAuto(List(propositionId1), List(Knowledge("案ずるより産むが易し。", "ja_JP", "{}", false )))
    assert(FacadeForAccessNeo4J.havePremiseNode(propositionId1) == false)
    val propositionId2 =  UUID.random.toString
    val knowledgeSentenceSet = KnowledgeSentenceSet(
      List(Knowledge("案ずるより産むが易し。","ja_JP", "{}")),
      List.empty[PropositionRelation],
      List(Knowledge("思い立ったが吉日。","ja_JP", "{}")),
      List.empty[PropositionRelation])
    Sentence2Neo4jTransformer.createGraph(propositionId2, knowledgeSentenceSet)
    assert(FacadeForAccessNeo4J.havePremiseNode(propositionId2) == true)
  }


}
