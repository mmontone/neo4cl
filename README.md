# neo4cl - a CL library for interacting with Neo4J

## Short description:
A very simple library that takes care of sending Cypher queries to a Neo4J server, and decoding the responses into something useful for processing in CL.


### What it does

- sends queries to a Neo4J server, and returns its responses
- very basic error reporting


### What it doesn't do

- anything involving Cypher itself. It takes a string, and assumes the server will know what to do with it.
- connection pooling. Each request is a whole new voyage of discovery. I did say it's simple.

It's organised around a `neo4j-rest-server` object, which holds the details needed for connecting to a Neo4J server. Its initargs are:
- protocol (optional: defaults to 'http')
- hostname (optional: defaults to 'localhost')
- port (optional: defaults to 7474)
- dbuser - the username with which you're authenticating to the Neo4J server 
- dbpasswd - the password with which you're authenticating to the Neo4J server 

It even comes with a suitably basic test-suite, in the package `neo4cl-test`, and it requires the FiveAM framework.


## Example usage:
We'll assume it's a default installation of Neo4J, so it's listening on `http://localhost:7474`, and the username and password are both 'neo4j'.
```
(defvar *server*
  (make-instance 'neo4cl:neo4j-rest-server'
                 :dbuser "neo4j"
                 :dbuser "neo4j"))

;; First, we change the password.
;; It changes the password stored in *server* for you, so it continues to "just work."
(neo4cl:change-password *server* "foobar")

;; Create some bloke called Andre
(neo4cl:neo4j-cypher-post-request
  *server*
  '((:query . "CREATE (n:Person { name : {name} }) RETURN n")
    (:params (:name . "Andre"))))

;; Is he there?
(neo4cl:neo4j-cypher-post-request
  *server*
  '((:query . "MATCH (n:Person {name: 'Andre'}) RETURN n.name")
    (:params (:name . "Andre"))))

;; We're bored; get rid of him
(neo4cl:neo4j-cypher-post-request
  *server*
  '((:query . "MATCH (n:Person {name: 'Andre'}) DELETE n")
    (:params (:name . "Andre"))))
```
