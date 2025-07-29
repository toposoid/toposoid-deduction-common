# toposoid-deduction-common
This is a common library used by toposoid developer in Toposoid projects.
Toposoid is a knowledge base construction platform.(see [Toposoid　Root Project](https://github.com/toposoid/toposoid.git))
In particular, this module is used by units that perform deductive reasoning in Toposoid projects.

[![Unit Test Action](https://github.com/toposoid/toposoid-deduction-common/actions/workflows/action.yml/badge.svg?branch=main)](https://github.com/toposoid/toposoid-deduction-common/actions/workflows/action.yml)

## Requirements
* Scala version 2.13.x,
* Sbt version 1.9.0
* The following microservices must be running
  * toposoid/toposoid-sentence-parser-japanese-web
  * toposoid/toposoid-common-nlp-japanese-web
  * toposoid/toposoid-sentence-parser-english-web
  * toposoid/toposoid-common-nlp-english-web
  * toposoid/corenlp
  * toposoid/data-accessor-weaviate
  * weaviate
  * toposoid/scala-data-accessor-neo4j-web
  * neo4j

## Setup
```bssh
sbt publishLocal
```
## Usage
```scala
//Defines convenient functions to access and extract information from Neo4j data during inference
import com.ideal.linked.toposoid.deduction.common.FacadeForAccessNeo4J._
//Defines convenient functions to access and extract information from Weaviate data during inference
import com.ideal.linked.toposoid.deduction.common.FacadeForAccessVectorDB._
//Define convenience functions for parsed data of propositions during inference
import com.ideal.linked.toposoid.deduction.common.AnalyzedSentenceObjectUtils._
//Definition of LocalNode's Duduction parent class
import com.ideal.linked.toposoid.deduction.common.DeductionUnitController
//Definition of SemiGlobalNode's Duduction parent class
import com.ideal.linked.toposoid.deduction.common.DeductionUnitControllerForSemiGlobal
```

# Note


## License
This program is offered under a commercial and under the AGPL license.
For commercial licensing, contact us at https://toposoid.com/contact.  For AGPL licensing, see below.

AGPL licensing:
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

## Author
* Makoto Kubodera([Linked Ideal LLC.](https://linked-ideal.com/))

Thank you!
