Neo4cl test suite
=================

Instructions for the very basic test suite:

It assumes there's a Docker image of Neo4j 4.3.9 running, with default settings except for the password. To make that happen, execute `./run_test_neo4j`.

Then, evaluate the following in an SBCL session:

```
(asdf:load-system :neo4cl-test)
(fiveam:run! 'neo4cl-test:main)
```

All tests are expected to pass. They did right before I added this README, anyway.
