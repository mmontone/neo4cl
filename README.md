# neo4cl - a CL library for interacting with Neo4J

## Short description:

A very simple library that takes care of sending Cypher queries to a Neo4J server, and decoding the responses into something useful for processing in CL.
Aims at compliance with Neo4J 3.0 via the HTTP API.


### What it does

- sends queries to a Neo4J server, and returns its responses
- very basic error reporting


### What it doesn't do

- anything involving Cypher itself. It forwards the query string, and assumes the server will know what to do with it.
- the BOLT protocol. I'll be happy to add it, but haven't even begun work on that.


### What it runs on

It's been tested so far on

- sbcl
- ccl
- ccl64


### How it works

It's organised around a `neo4j-rest-server` object, which holds the details needed for connecting to a Neo4J server. Its initargs and defaults are:

- :protocol - default = "http", but can be whatever Drakma will accept
- :hostname - default = "localhost"
- :port - default = 7474
- :dbuser - default = "neo4j"
- :dbpasswd - default = "neo4j"

It comes with a suitably basic test-suite, in the package `neo4cl-test`, which requires the FiveAM framework.


## Example usage:

We'll assume it's a default installation of Neo4J, so it's listening on `http://localhost:7474`, and the username and password are both 'neo4j'.
```
(defvar *server*
  (make-instance 'neo4cl:neo4j-rest-server))

;; First, we change the password.
;; It changes the password stored in *server* for you, so it continues to "just work."
(neo4cl:change-password *server* "foobar")

;; Create some bloke called Andre
(neo4cl:neo4j-transaction
  *server*
  `((:STATEMENTS
      ((:STATEMENT . "CREATE (n:Person { name : {name} }) RETURN n")
       (:PARAMETERS .
                    ((:properties .
                                  ((:name . "Andre")))))))))

;; Is he there?
(neo4cl:neo4j-transaction
  *server*
  `((:STATEMENTS
      ((:STATEMENT . "MATCH (x:Person {name: 'Andre'}) RETURN x.name")))))

;; We're bored; get rid of him
(neo4cl:neo4j-transaction
  *server*
  `((:STATEMENTS
      ((:STATEMENT . "MATCH (x:Person {name: 'Andre'}) DELETE x")))))
```
