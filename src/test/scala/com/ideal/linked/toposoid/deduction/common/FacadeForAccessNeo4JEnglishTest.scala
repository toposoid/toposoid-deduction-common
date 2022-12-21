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
import com.ideal.linked.toposoid.protocol.model.base.AnalyzedSentenceObject
import com.ideal.linked.toposoid.protocol.model.neo4j.Neo4jRecords
import com.ideal.linked.toposoid.sentence.transformer.neo4j.Sentence2Neo4jTransformer
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, DiagrammedAssertions, FlatSpec}
import play.api.libs.json.Json
import io.jvm.uuid.UUID

class FacadeForAccessNeo4JEnglishTest extends FlatSpec with DiagrammedAssertions with BeforeAndAfter with BeforeAndAfterAll{

  override def beforeAll(): Unit = {
    Neo4JAccessor.delete()
    Sentence2Neo4jTransformer.createGraphAuto(List(UUID.random.toString), List(Knowledge("Time is money.","en_US", "{}", false )))
  }

  override def afterAll(): Unit = {
    Neo4JAccessor.delete()
  }

  "A query for english knowledge" should "be handled properly" in {
    val query:String = "MATCH (n) WHERE n.lang='en_US' RETURN n"
    val result:String = FacadeForAccessNeo4J.getCypherQueryResult(query, "")
    val neo4jRecords: Neo4jRecords = Json.parse(result).as[Neo4jRecords]
    val sentenceMap: List[(Int, String)] = neo4jRecords.records.reverse.map(record => {
      record.filter(x => x.key.equals("n")).map(y =>
        y.value.logicNode.currentId -> y.value.logicNode.surface
      ).head
    })
    val sentence: String = sentenceMap.toSeq.sortBy(_._1).foldLeft("") { (acc, x) => acc + " " + x._2 }
    assert(sentence.trim.equals("Time is money ."))

  }

  "Neo4j data" should "be properly converted to AnalyzedSentenceObject Type" in {
    val propositionId =  UUID.random.toString
    Sentence2Neo4jTransformer.createGraphAuto(List(propositionId, propositionId), List(Knowledge("Time is money.","en_US", "{}", false ), Knowledge("Fear often exaggerates danger.","en_US", "{}")))
    val asos = FacadeForAccessNeo4J.neo4JData2AnalyzedSentenceObjectByPropositionId(propositionId, 1)
    assert(asos.analyzedSentenceObjects.size == 2)
    asos.analyzedSentenceObjects.foreach(aso => {
      assert(AnalyzedSentenceObjectUtils.makeSentence(aso).get(1).get.sentence == "Time is money ." || AnalyzedSentenceObjectUtils.makeSentence(aso).get(1).get.sentence == "Fear often exaggerates danger .")
    })

  }

  "havePremiseNode" should "be handled properly" in {
    val propositionId1 =  UUID.random.toString
    Sentence2Neo4jTransformer.createGraphAuto(List(propositionId1), List(Knowledge("Time is money.","en_US", "{}", false )))
    assert(FacadeForAccessNeo4J.havePremiseNode(propositionId1) == false)
    val propositionId2 =  UUID.random.toString
    val knowledgeSentenceSet = KnowledgeSentenceSet(
      List(Knowledge("Fear often exaggerates danger.","en_US", "{}")),
      List.empty[PropositionRelation],
      List(Knowledge("Grasp Fortune by the forelock.","en_US", "{}")),
      List.empty[PropositionRelation])
    Sentence2Neo4jTransformer.createGraph(propositionId2, knowledgeSentenceSet)
    assert(FacadeForAccessNeo4J.havePremiseNode(propositionId2) == true)
  }


}
