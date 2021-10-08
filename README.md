# toposoid-deduction-common
This is a common library used by toposoid developer in Toposoid projects.
Toposoid is a knowledge base construction platform.(see [Toposoid　Root Project](https://github.com/toposoid/toposoid.git))
In particular, this module is used by units that perform deductive reasoning in Toposoid projects.

[![Unit Test Action](https://github.com/toposoid/toposoid-deduction-common/actions/workflows/action.yml/badge.svg?branch=main)](https://github.com/toposoid/toposoid-deduction-common/actions/workflows/action.yml)

## Requirements
* Docker version 20.10.x, or later
* docker-compose version 1.22.x

## Setup
```bssh
sbt publishLocal
```
## Usage
```scala
import com.ideal.linked.toposoid.deduction.common.FacadeForAccessNeo4J._
```

# Note


## License
toposoid/toposoid-deduction-common is Open Source software released under the [Apache 2.0 license](https://www.apache.org/licenses/LICENSE-2.0.html).

## Author
* Makoto Kubodera([Linked Ideal LLC.](https://linked-ideal.com/))

Thank you!
