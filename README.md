# neo4cl - a CL library for interacting with Neo4J

# Short description:

From the [Neo4J website](http://neo4j.com/): "Neo4j is a highly scalable native graph database."

Graph databases emphasise the relationships between things, and the information to be found there, in contrast to the relational focus on the things themselves. They are typically much more flexible in their definitions of things than RDBMSes, which fits well with Lisp's fluid approach to functionality.

Neo4J is a very popular graph database that scales well. It implements a property graph, which means you can assign attributes to relationships as well as to nodes, and its query language (Cypher) is very expressive. It's an excellent transactional database which satisfies the ACID model, in contrast to something like RDF, which is better suited to data warehousing where responsiveness is traded off for more semantic richness.

Neo4CL is very simple library that sends Cypher queries to a Neo4J server, and decodes the responses into something useful for processing in CL. The queries and their responses take the form of alists. This library aims at compliance with Neo4J 4.3.9 via both the HTTP API and the Bolt protocol, based on the documentation in the [Neo4J developer manual](http://neo4j.com/docs/developer-manual/current/#http-api-index), and is intended as something to build applications on, more than for interactive use.

The HTTP API is deprecated, and will be dropped from this codebase altogether as soon as the Bolt-client implementation matches it in maturity, unless somebody actually tells me they're using it. To contact me, please email me: `james` at `sysc.at`.


## Status

### Bolt client

Alpha: it works, but not all functionality is implemented yet, and it hasn't been fully production-tested.

Working:

- Basic authentication.
- Autocommit transactions.
- Multi-chunk message parsing.
    - I.e, it will reassemble multi-chunk messages _received from_ the server in response to queries.
- Lisp -> Packstream implementation:
    - `nil` -> `Null`
    - `Boolean`
    - `string` -> `String`
    - 8-bit integers -> `Integer`
        - Longer integers will be supported in future.
    - `list` -> `List`
    - `hash-table` -> `Dictionary`
- Packstream -> Lisp implementation:
    - `Null` -> `nil`
    - `Boolean` -> `t` and `nil`
    - `TINYINT` -> `integer`
    - `String` -> `string`
    - `List` -> `list`
    - `Dictionary` -> `alist`
        - `Dictionary` is defined to potentially return multiple values for the same key; alists support this, but hash-tables don't.
    - Structures are parsed into CL classes, whose symbols are exported from the package:
        - `Node` -> `node`
        - `Relationship` -> `relationship`


Not yet implemented:

- Explicit transactions.
- Routing.
- Multi-chunk encoding of large client messages.
- Packstream parsing _not_ implemented:
    - `Float`
    - `Bytes`
    - `Structure` types other than `Node` and `Relationship`
    - Negative integers are not implemented for values greater than 8 bits.
- TLS/SSL connections (yet).


Not implemented, and not on the roadmap:

- Anything involving Cypher itself. It forwards the query string, and assumes the server will know what to do with it.
- Any kind of object-graph mapping.


### HTTP client

Beta, deprecated.

- Working, to at least a basic degree.
- Not compatible with Neo4j 4.4.
    - This will not be fixed, because it's being replaced by the Bolt client.


## What it does

- Execute basic queries as autocommit transactions, parsing the results into suitable CL types, including various classes..


## What it doesn't do



# Compatibility

## Neo4j versions

### Bolt client

4.3.9


### HTTP client

4.1.0 -> 4.3.9


## Lisp compatibility

SBCL only.

Once it's fully functional for Neo4j 4.3 and 4.4, support may be added for other implementations, if any interest is shown.


## Dependencies


### Bolt client

All available via Quicklisp:

- `fiveam` for testing
- `trivial-utf-8`
- `usocket`


### HTTP client

All available via Quicklisp:

- `cl-base64`
- `cl-json`
- `cl-ppcre`
- `drakma`
- `flexi-streams`


# Bolt client: how it works

Clients mostly interact with a `bolt-session` object, which contains objects representing the `usocket` connection stream, the version of the Bolt protocol in use for this session, and the version of the Neo4j database that it's connected to.


## Example usage

Fetch the label and `name` property of all nodes in the database:

    (defparameter *bolt-server*
      (make-instance 'neo4cl::bolt-server
                     :hostname "192.0.2.1"))
    
    (defparameter *bolt-auth-basic*
      (make-instance 'neo4cl::bolt-auth-basic
                     :username "neo4j"
                     :password "wallaby"))
    
    
    (let ((session (neo4cl:establish-bolt-session *bolt-server* *bolt-auth-basic*)))
      (neo4cl:bolt-transaction-autocommit
        session
        "MATCH (p) RETURN p.name AS name, LABELS(p) AS label")
      (neo4cl:disconnect session))


## Notes about Packstream encoding

[Packstream](https://7687.org/packstream/packstream-specification-1.html) is the message-encoding format used by the Bolt protocol.

It has explicit encoding for Boolean and Null values, but Common Lisp does not; this makes it difficult for the encoder to figure out whether the value for a hash-table entry is null, an empty list, or a boolean False. To resolve this, `neo4cl` recognises the values `:true`, `:false` and `:null`, and encodes them accordingly.


## Error/condition handling

Three errors and one non-error condition are defined, following the [Neo4J classifications](http://neo4j.com/docs/developer-manual/current/reference/#status-codes):

- client-error
- transient-error
- database-error
- client-notification

They're further broken down into Category and Title, by simply splitting on the dot, and the Message is also included.

E.g, `Neo.ClientError.Schema.ConstraintValidationFailed` would be raised as
```
(error neo4cl:client-error
    :category "Schema"
    :title "ConstraintValidationFailed"
    :message "Node 126 already exists with label routers and property \"uid\"=[whitesands]")
```

The symbols `category`, `title` and `message` are exported so these fields can be read.
