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

import com.ideal.linked.toposoid.common.{Neo4JUtilsImpl, TransversalState}
import com.ideal.linked.toposoid.knowledgebase.regist.model.{Knowledge, PropositionRelation}
import com.ideal.linked.toposoid.protocol.model.base.AnalyzedSentenceObjects
import com.ideal.linked.toposoid.protocol.model.neo4j.Neo4jRecords
import com.ideal.linked.toposoid.protocol.model.parser.{KnowledgeForParser, KnowledgeSentenceSetForParser}
import com.ideal.linked.toposoid.test.utils.TestUtils
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll}
import org.scalatest.flatspec.AnyFlatSpec
import play.api.libs.json.Json
import io.jvm.uuid.UUID

class FacadeForAccessNeo4JJapaneseTest extends AnyFlatSpec with BeforeAndAfter with BeforeAndAfterAll{

  val transversalState = TransversalState(userId="test-user", username="guest", roleId=0, csrfToken = "")
  val neo4JUtils = new Neo4JUtilsImpl()
  def deleteNeo4JAllData(transversalState: TransversalState): Unit = {
    val query = "MATCH (n) OPTIONAL MATCH (n)-[r]-() DELETE n,r"
    neo4JUtils.executeQuery(query, transversalState)
  }


  override def beforeAll(): Unit = {
    deleteNeo4JAllData(transversalState)
    val knowledgeForParser = KnowledgeForParser(UUID.random.toString, UUID.random.toString, Knowledge("案ずるより産むが易し。", "ja_JP", "{}", false ))
    val knowledgeSentenceSetForParser = KnowledgeSentenceSetForParser(
      premiseList = List.empty[KnowledgeForParser],
      premiseLogicRelation = List.empty[PropositionRelation],
      claimList = List(knowledgeForParser),
      claimLogicRelation = List.empty[PropositionRelation])
    TestUtils.registerData(knowledgeSentenceSetForParser, transversalState, addVectorFlag = false)

  }

  override def afterAll(): Unit = {
    deleteNeo4JAllData(transversalState)
  }

  "A query for japanese knowledge" should "be handled properly" in {
    val query:String = "MATCH (n) WHERE n.lang='ja_JP' RETURN n"
    val result:String = FacadeForAccessNeo4J.getCypherQueryResult(query, "", transversalState)
    val neo4jRecords: Neo4jRecords = Json.parse(result).as[Neo4jRecords]

    val sentenceMap: List[(Int, String)] = neo4jRecords.records.reverse.foldLeft(List.empty[(Int, String)]) {
      (acc, record) => {
        acc ::: record.filter(x => x.key.equals("n")).foldLeft(List.empty[(Int, String)]) {
          (acc2, y) => {
            y.value.localNode match {
              case Some(z) => acc2 :+ (z.predicateArgumentStructure.currentId -> z.predicateArgumentStructure.surface)
              case _ => acc2
            }
          }
        }
      }
    }
    val sentence: String = sentenceMap.toSeq.sortBy(_._1).foldLeft("") { (acc, x) => acc + x._2 }
    assert(sentence.equals("案ずるより産むが易し。"))
  }

  "Neo4j data" should "be properly converted to AnalyzedSentenceObject Type" in {
    val propositionId =  UUID.random.toString
    val sentenceId1 =  UUID.random.toString
    val sentenceId2 =  UUID.random.toString
    val knowledgeSentenceSetForParser = KnowledgeSentenceSetForParser(
      List.empty[KnowledgeForParser],
      List.empty[PropositionRelation],
      List(KnowledgeForParser(propositionId, sentenceId1, Knowledge("案ずるより産むが易し。", "ja_JP", "{}", false )), KnowledgeForParser(propositionId, sentenceId2, Knowledge("思い立ったが吉日。", "ja_JP", "{}", false ))),
      List.empty[PropositionRelation])
    TestUtils.registerData(knowledgeSentenceSetForParser, transversalState, addVectorFlag = false)
    val asos:AnalyzedSentenceObjects = FacadeForAccessNeo4J.neo4JData2AnalyzedSentenceObjectByPropositionId(propositionId, 1, transversalState)
    assert(asos.analyzedSentenceObjects.size == 2)
    asos.analyzedSentenceObjects.foreach(aso => {
      assert(AnalyzedSentenceObjectUtils.makeSentence(aso).get(1).get.sentence == "案ずるより産むが易し。" || AnalyzedSentenceObjectUtils.makeSentence(aso).get(1).get.sentence == "思い立ったが吉日。")
    })
  }

  "Neo4j data with logic relation" should "be properly converted to AnalyzedSentenceObject Type" in {
    val propositionId =  UUID.random.toString
    val sentenceId1 =  UUID.random.toString
    val sentenceId2 =  UUID.random.toString
    val sentenceId3 =  UUID.random.toString
    val sentenceId4 =  UUID.random.toString

    val sentenceA = "案ずるより産むが易し。"
    val sentenceB = "思い立ったが吉日。"
    val sentenceC = "時は金なり。"
    val sentenceD = "人事を尽くして天命を待つ。"
    val knowledge1 = Knowledge(sentenceA,"ja_JP", "{}", false)
    val knowledge2 = Knowledge(sentenceB,"ja_JP", "{}", false)
    val knowledge3 = Knowledge(sentenceC,"ja_JP", "{}", false)
    val knowledge4 = Knowledge(sentenceD,"ja_JP", "{}", false)

    val knowledgeSentenceSetForParser = KnowledgeSentenceSetForParser(
      List(KnowledgeForParser(propositionId, sentenceId1, knowledge1), KnowledgeForParser(propositionId, sentenceId2, knowledge2)),
      List(PropositionRelation("AND", 0,1)),
      List(KnowledgeForParser(propositionId, sentenceId3, knowledge3), KnowledgeForParser(propositionId, sentenceId4, knowledge4)),
      List(PropositionRelation("AND", 0,1)))
    TestUtils.registerData(knowledgeSentenceSetForParser, transversalState, addVectorFlag = false)
    val asos1:AnalyzedSentenceObjects = FacadeForAccessNeo4J.neo4JData2AnalyzedSentenceObjectByPropositionId(propositionId, 0, transversalState)
    val asos2:AnalyzedSentenceObjects = FacadeForAccessNeo4J.neo4JData2AnalyzedSentenceObjectByPropositionId(propositionId, 1, transversalState)
    assert(asos1.analyzedSentenceObjects.size == 2)
    asos1.analyzedSentenceObjects.foreach(aso => {
      assert(AnalyzedSentenceObjectUtils.makeSentence(aso).get(0).get.sentence == "案ずるより産むが易し。" || AnalyzedSentenceObjectUtils.makeSentence(aso).get(0).get.sentence == "思い立ったが吉日。")
    })
    assert(asos2.analyzedSentenceObjects.size == 2)
    asos2.analyzedSentenceObjects.foreach(aso => {
      assert(AnalyzedSentenceObjectUtils.makeSentence(aso).get(1).get.sentence == "時は金なり。" || AnalyzedSentenceObjectUtils.makeSentence(aso).get(1).get.sentence == "人事を尽くして天命を待つ。")
    })
  }

}
