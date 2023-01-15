# toposoid-deduction-common
This is a common library used by toposoid developer in Toposoid projects.
Toposoid is a knowledge base construction platform.(see [Toposoid　Root Project](https://github.com/toposoid/toposoid.git))
In particular, this module is used by units that perform deductive reasoning in Toposoid projects.

[![Unit Test Action](https://github.com/toposoid/toposoid-deduction-common/actions/workflows/action.yml/badge.svg?branch=main)](https://github.com/toposoid/toposoid-deduction-common/actions/workflows/action.yml)

## Requirements
* Scala version 2.12.x,
* Sbt version 1.2.8
* The following microservices must be running
> toposoid/toposoid-sentence-parser-japanese-web
> toposoid/toposoid-common-nlp-japanese-web
> toposoid/toposoid-sentence-parser-english-web
> toposoid/toposoid-common-nlp-english-web
> toposoid/corenlp:3.4.2-workflow
> toposoid/scala-data-accessor-neo4j-web
> neo4j:4.1.3

## Setup
```bssh
sbt publishLocal
```
## Usage
```scala
//Defines convenient functions to access and extract information from Neo4j data during inference
import com.ideal.linked.toposoid.deduction.common.FacadeForAccessNeo4J._
//Define convenience functions for parsed data of propositions during inference
import com.ideal.linked.toposoid.deduction.common.AnalyzedSentenceObjectUtils._
```

# Note


## License
toposoid/toposoid-deduction-common is Open Source software released under the [Apache 2.0 license](https://www.apache.org/licenses/LICENSE-2.0.html).

## Author
* Makoto Kubodera([Linked Ideal LLC.](https://linked-ideal.com/))

Thank you!
