(asdf:defsystem #:neo4cl
  :serial t
  :description "Basic library for interacting with Neo4J"
  :author "James Fleming <james@electronic-quill.net>"
  :license "Unlicensed, so far."
  :depends-on (#:cl-ppcre ; Check UUID formatting and identify error strings
               #:drakma  ; Send requests to the neo4j server
               #:cl-json ; Encode/decode json requests
               #:babel   ; Convert neo4j's octet response into a string
               #:flexi-streams ; Convert between strings and octets
               #:cl-base64  ; base64 encoding/decoding
               )
  :components ((:file "neo4cl")))

(defpackage #:neo4cl
  (:use
    #:cl)
  (:export neo4j-rest-server
           change-password
           neo4j-transaction
           neo4j-transaction-error
           ))
