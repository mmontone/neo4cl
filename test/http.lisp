;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the Apache License, Version 2.0 (the "License");
;   you may not use this file except in compliance with the License.
;   You may obtain a copy of the License at
;
;       http://www.apache.org/licenses/LICENSE-2.0
;
;   Unless required by applicable law or agreed to in writing, software
;   distributed under the License is distributed on an "AS IS" BASIS,
;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;   See the License for the specific language governing permissions and
;   limitations under the License.


;;;; Test suite for HTTP driver


(in-package #:neo4cl-test)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


(fiveam:def-suite http
                  :description "Tests for the HTTP driver."
                  :in main)

(fiveam:in-suite http)


(fiveam:test
  character-encoding
  "Regression tests for non-ASCII character-encoding problems."
  ;; The original
  (let ((test-string-1 "Röver Edwárd Petrusky the fourth"))
    (fiveam:is (equal
                 test-string-1
                 (cl-json:decode-json-from-string
                   (cl-json:encode-json-to-string test-string-1))))))

(fiveam:test
  neo4j-base-api
  "Test the lowest-level methods for interacting with Neo4j"
  ;; Can we authenticate?
  (multiple-value-bind (body numeric verbal headers)
    (neo4cl:neo4j-transaction
      *http-server*
      '((:statements ((:statement . "MATCH (n) RETURN n")))))
    (declare (ignore body)
             (ignore headers))
    (fiveam:is (equal 200 numeric))
    (fiveam:is (equal "OK" verbal)))
  ;; Store a node
  (fiveam:is (listp (neo4cl:neo4j-transaction
                      *http-server*
                      `((:STATEMENTS
                          ((:STATEMENT . "CREATE (n:Person $properties) RETURN n")
                           (:PARAMETERS .
                                        ((:properties .
                                                      ((:name . "Andre")))))))))))
  ;; Retrieve a node
  (fiveam:is (equal
               "Andre"
               (neo4cl:extract-data-from-get-request
                 (neo4cl:neo4j-transaction
                   *http-server*
                   '((:STATEMENTS
                       ((:STATEMENT . "MATCH (x:Person {name: 'Andre'}) RETURN x.name"))))))))
  ;; Delete a node
  (let ((result (neo4cl:neo4j-transaction
                  *http-server*
                  `((:STATEMENTS
                      ((:STATEMENT . "MATCH (x:Person {name: 'Andre'}) DELETE x")))))))
    (fiveam:is (listp result))
    (fiveam:is (equal :RESULTS (car (first result))))
    (fiveam:is (equal :ERRORS (car (second result))))))
