;   Copyright 2022 James Fleming <james@electronic-quill.net>
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


;;;; Test suite for the Bolt driver


(in-package #:neo4cl-test)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


(fiveam:def-suite bolt
                  :description "Tests for the Bolt driver."
                  :in main)

(fiveam:in-suite bolt)


;;;; Pure (enough) functions

(fiveam:test
  read-bolt-message-from-stream
  "Check that this function works as expected."
  (let ((server-hello-1
          ;; Transcription of actual hello message received from a Neo4j 3.9 server
          (vector #x00 #x34
                  #xb1 #x70 #xa3 #x86 #x73 #x65 #x72 #x76 #x65 #x72 #x8b #x4e #x65 #x6f
                  #x34 #x6a #x2f #x34 #x2e #x33 #x2e #x39 #x8d #x63 #x6f #x6e #x6e #x65 #x63 #x74
                  #x69 #x6f #x6e #x5f #x69 #x64 #x88 #x62 #x6f #x6c #x74 #x2d #x31 #x33 #x30 #x85
                  #x68 #x69 #x6e #x74 #x73 #xa0
                  #x00 #x00))
        (bolt-4-3 (make-instance 'neo4cl::bolt-protocol-4-3)))
    (fiveam:is
      (equalp (vector
                #xb1 #x70 #xa3 #x86 #x73 #x65 #x72 #x76 #x65 #x72 #x8b #x4e #x65 #x6f
                #x34 #x6a #x2f #x34 #x2e #x33 #x2e #x39 #x8d #x63 #x6f #x6e #x6e #x65 #x63 #x74
                #x69 #x6f #x6e #x5f #x69 #x64 #x88 #x62 #x6f #x6c #x74 #x2d #x31 #x33 #x30 #x85
                #x68 #x69 #x6e #x74 #x73 #xa0)
              (neo4cl::read-bolt-message-from-stream
                (flexi-streams:make-in-memory-input-stream server-hello-1)
                bolt-4-3)))))


;;;; Side-effecting functions

(fiveam:test
  connect
  "Test the Bolt connect-and-handshake function."
  :depends-on 'packstream
  ;; Make a connection, and hopefully get a 2-element list in return.
  (let ((details (neo4cl:connect *bolt-server*)))
    ;; First element should be a usocket stream object
    (fiveam:is (equal 'usocket:stream-usocket (type-of (first details))))
    ;; Second element should be a subtype of bolt-protocol
    (fiveam:is (equal 'neo4cl:bolt-protocol-4-3 (type-of (second details))))
    ;; Now check finger details of the protocol version
    (let ((received-version (second details))
          (expected-version (make-instance 'neo4cl:bolt-protocol-4-3)))
      (fiveam:is (equal (neo4cl:major-version expected-version)
                        (neo4cl:major-version received-version)))
      (fiveam:is (equal (neo4cl:minor-version expected-version)
                        (neo4cl:minor-version received-version)))
      (usocket:socket-close (first details)))))

(fiveam:test
  bolt-hello-goodbye
  "Test session setup and teardown."
  :depends-on 'connect
  ;; Create the session, and get (hopefully) a bolt-session object
  (let ((session (neo4cl:establish-bolt-session *bolt-server*)))
    (fiveam:is (equal 'neo4cl:bolt-session (type-of session)))
    (fiveam:is (equal 'usocket:stream-usocket (type-of (neo4cl:connection session))))
    ;; Clean up after ourselves
    (neo4cl:disconnect session)))

;;FIXME: test parameters, with both a dictionary and an alist
(fiveam:test
  autocommit-transaction
  "Transactions of the auto-commit variety."
  :depends-on 'bolt-hello-goodbye
  (let ((session (neo4cl:establish-bolt-session *bolt-server*)))
    ;; Basic queries
    (fiveam:is (null (neo4cl:bolt-transaction-autocommit session "CREATE (:foo {name: \"bar\"})")))
    (fiveam:is (null (neo4cl:bolt-transaction-autocommit session "CREATE (:foo {name: \"baz\"})")))
    (let ((result (neo4cl:bolt-transaction-autocommit
                    session
                    "MATCH (p:foo) RETURN p.name AS name, LABELS(p) AS label")))
      (fiveam:is (= 2 (length result)))
      (fiveam:is (equalp '((("name" . "bar") ("label" . "foo"))
                           (("name" . "baz") ("label" . "foo")))
                         result)))
    (fiveam:is (null (neo4cl:bolt-transaction-autocommit session "MATCH (p:foo) DELETE p")))
    ;; Parameterised CREATE - hash-table edition
    (let ((params (make-hash-table :test #'equal)))
      (setf (gethash "colour" params) "blue")
      (fiveam:is (null (neo4cl:bolt-transaction-autocommit
                         session
                         "CREATE (:baz {colour: $colour})"
                         :parameters params))))
    (fiveam:is (equal '((("labels" . "baz") ("colour" . "blue")))
                      (neo4cl:bolt-transaction-autocommit
                        session
                        "MATCH (p:baz) RETURN LABELS(p) AS labels, p.colour AS colour")))
    (fiveam:is (null (neo4cl:bolt-transaction-autocommit session "MATCH (p:baz) DELETE p")))
    ;; Parameterised CREATE - alist edition
    (fiveam:is (null (neo4cl:bolt-transaction-autocommit
                       session
                       "CREATE (:bar {value: $val})"
                       :parameters '(("val" . "quux")))))
    (fiveam:is (equal '((("label" . "bar") ("val" . "quux")))
                      (neo4cl:bolt-transaction-autocommit
                        session
                        "MATCH (p:bar) RETURN LABELS(p) AS label, p.value AS val")))
    (fiveam:is (null (neo4cl:bolt-transaction-autocommit session "MATCH (p:bar) DELETE p")))
    ;; Clean up
    (neo4cl::log-message :debug "Disconnecting session.")
    (neo4cl:disconnect session)))

(fiveam:test
  round-trip-encoding
  "Confirm that values come back out the same way they went in."
  :depends-on 'autocommit-transaction
  ;; Establish the session
  (let ((session (neo4cl:establish-bolt-session *bolt-server*)))
    ;; String - double-quoted
    (neo4cl:bolt-transaction-autocommit session "CREATE (:foo {name: \"bar\"})")
    (fiveam:is (equal '((("name" . "bar") ("label" . "foo")))
                      (neo4cl:bolt-transaction-autocommit
                        session
                        "MATCH (p:foo) RETURN p.name AS name, LABELS(p) AS label")))
    (neo4cl:bolt-transaction-autocommit session "MATCH (p:foo) DELETE p")
    ;; String - single-quoted
    (neo4cl:bolt-transaction-autocommit session "CREATE (:foo {name: 'bar'})")
    (fiveam:is (equal '((("name" . "bar") ("label" . "foo")))
                      (neo4cl:bolt-transaction-autocommit
                        session
                        "MATCH (p:foo) RETURN p.name AS name, LABELS(p) AS label")))
    (neo4cl:bolt-transaction-autocommit session "MATCH (p:foo) DELETE p")
    ;; String - parameterised
    (neo4cl:bolt-transaction-autocommit session "CREATE (:foo {name: $name})"
                                        :parameters '(("name" . "bar")))
    (fiveam:is (equal '((("name" . "bar") ("label" . "foo")))
                      (neo4cl:bolt-transaction-autocommit
                        session
                        "MATCH (p:foo) RETURN p.name AS name, LABELS(p) AS label")))
    (neo4cl:bolt-transaction-autocommit session "MATCH (p:foo) DELETE p")
    ;; Positive tiny int
    (neo4cl:bolt-transaction-autocommit session "CREATE (:foo {name: $name})"
                                        :parameters '(("name" . 123)))
    (fiveam:is (equal '((("name" . 123) ("label" . "foo")))
                      (neo4cl:bolt-transaction-autocommit
                        session
                        "MATCH (p:foo) RETURN p.name AS name, LABELS(p) AS label")))
    (neo4cl:bolt-transaction-autocommit session "MATCH (p:foo) DELETE p")
    ;; Negative tiny int
    (neo4cl:bolt-transaction-autocommit session "CREATE (:foo {name: $name})"
                                        :parameters '(("name" . -123)))
    (fiveam:is (equal '((("name" . -123) ("label" . "foo")))
                      (neo4cl:bolt-transaction-autocommit
                        session
                        "MATCH (p:foo) RETURN p.name AS name, LABELS(p) AS label")))
    (neo4cl:bolt-transaction-autocommit session "MATCH (p:foo) DELETE p")
    ;; Positive 16-bit int
    (neo4cl:bolt-transaction-autocommit session "CREATE (:foo {name: $name})"
                                        :parameters '(("name" . 1234)))
    (fiveam:is (equal '((("name" . 1234) ("label" . "foo")))
                      (neo4cl:bolt-transaction-autocommit
                        session
                        "MATCH (p:foo) RETURN p.name AS name, LABELS(p) AS label")))
    (neo4cl:bolt-transaction-autocommit session "MATCH (p:foo) DELETE p")
    ;; Negative 16-bit int
    (neo4cl:bolt-transaction-autocommit session "CREATE (:foo {name: $name})"
                                        :parameters '(("name" . -4268)))
    (fiveam:is (equal '((("name" . -4268) ("label" . "foo")))
                      (neo4cl:bolt-transaction-autocommit
                        session
                        "MATCH (p:foo) RETURN p.name AS name, LABELS(p) AS label")))
    (neo4cl:bolt-transaction-autocommit session "MATCH (p:foo) DELETE p")
    ;; Positive 64-bit int
    (neo4cl:bolt-transaction-autocommit session "CREATE (:foo {name: $name})"
                                        :parameters '(("name" . 8875584498032208147 )))
    (fiveam:is (equal '((("name" . 8875584498032208147) ("label" . "foo")))
                      (neo4cl:bolt-transaction-autocommit
                        session
                        "MATCH (p:foo) RETURN p.name AS name, LABELS(p) AS label")))
    (neo4cl:bolt-transaction-autocommit session "MATCH (p:foo) DELETE p")
    ;; Negative 64-bit int
    (neo4cl:bolt-transaction-autocommit session "CREATE (:foo {name: $name})"
                                        :parameters '(("name" . -9223372036854775808)))
    (fiveam:is (equal '((("name" . -9223372036854775808) ("label" . "foo")))
                      (neo4cl:bolt-transaction-autocommit
                        session
                        "MATCH (p:foo) RETURN p.name AS name, LABELS(p) AS label")))
    (neo4cl:bolt-transaction-autocommit session "MATCH (p:foo) DELETE p")
    ;; Positive double-float
    (neo4cl:bolt-transaction-autocommit session "CREATE (:foo {val: $float})"
                                        :parameters '(("float" . 1.23)))
    ;; Note that the return value is *specifically* a double-float
    (fiveam:is (equal '((("val" . 1.2300000190734863d0)))
                      (neo4cl:bolt-transaction-autocommit
                        session
                        "MATCH (p:foo) RETURN p.val AS val")))
    (neo4cl:bolt-transaction-autocommit session "MATCH (p:foo) DELETE p")
    ;; Negative double-float
    (neo4cl:bolt-transaction-autocommit session "CREATE (:foo {val: $float})"
                                        :parameters '(("float" . -1.23)))
    ;; Note that the return value is *specifically* a double-float
    (fiveam:is (equal '((("val" . -1.2300000190734863d0)))
                      (neo4cl:bolt-transaction-autocommit
                        session
                        "MATCH (p:foo) RETURN p.val AS val")))
    (neo4cl:bolt-transaction-autocommit session "MATCH (p:foo) DELETE p")
    ;; Date structure
    (let ((testval (make-instance 'date :days 123)))
      (neo4cl:bolt-transaction-autocommit session
                                          "CREATE (:foo { date: $date })"
                                          :parameters `(("date" . ,testval)))
      (let ((result (cdr (assoc "date"
                                (car (neo4cl:bolt-transaction-autocommit
                                       session
                                       "MATCH (p:foo) RETURN p.date as date"))
                                :test #'equal))))
        (fiveam:is (equalp (neo4cl::days testval) (neo4cl::days result)))))
    (neo4cl:bolt-transaction-autocommit session "MATCH (p:foo) DELETE p")
    ;; Duration structure
    (let ((testval (make-instance 'duration
                                  :months 1
                                  :days 1
                                  :seconds 24
                                  :nanoseconds 1234)))
      (neo4cl:bolt-transaction-autocommit session
                                          "CREATE (:foo { duration: $duration })"
                                          :parameters `(("duration" . ,testval)))
      (let ((result (cdr (assoc "duration"
                                (car (neo4cl:bolt-transaction-autocommit
                                       session
                                       "MATCH (p:foo) RETURN p.duration as duration"))
                                :test #'equal))))
        (fiveam:is (equalp (neo4cl::months testval) (neo4cl::months result)))
        (fiveam:is (equalp (neo4cl::days testval) (neo4cl::days result)))
        (fiveam:is (equalp (neo4cl::seconds testval) (neo4cl::seconds result)))
        (fiveam:is (equalp (neo4cl::nanoseconds testval) (neo4cl::nanoseconds result)))))
    (neo4cl:bolt-transaction-autocommit session "MATCH (p:foo) DELETE p")
    ;; Point2d structure
    ;; From https://metacpan.org/pod/Neo4j::Types::Point#srid I get this reference:
    ;; "To date, Neo4j has defined four SRIDs: 4326, 4979, 7203, and 9157"
    ;; Of those, 4326 and 7203 appear to be 2D, while 4979 and 9157 are 3D.
    (let ((testval (make-instance 'point2d
                                  :srid 4326
                                  :x 1.23
                                  :y 2.34)))
      (neo4cl:bolt-transaction-autocommit session
                                          "CREATE (:foo { point: $point })"
                                          :parameters `(("point" . ,testval)))
      (let ((result (cdr (assoc "point"
                                (car (neo4cl:bolt-transaction-autocommit
                                       session
                                       "MATCH (p:foo) RETURN p.point as point"))
                                :test #'equal))))
        (fiveam:is (equalp (neo4cl::srid testval) (neo4cl::srid result)))
        (fiveam:is (equalp (neo4cl::x testval) (neo4cl::x result)))
        (fiveam:is (equalp (neo4cl::y testval) (neo4cl::y result)))))
    (neo4cl:bolt-transaction-autocommit session "MATCH (p:foo) DELETE p")
    ;; Point3d structure
    (let ((testval (make-instance 'point3d
                                  :srid 9157
                                  :x 1.23
                                  :y 2.34
                                  :z 3.45)))
      (neo4cl:bolt-transaction-autocommit session
                                          "CREATE (:foo { point: $point })"
                                          :parameters `(("point" . ,testval)))
      (let ((result (cdr (assoc "point"
                                (car (neo4cl:bolt-transaction-autocommit
                                       session
                                       "MATCH (p:foo) RETURN p.point as point"))
                                :test #'equal))))
        (fiveam:is (equalp (neo4cl::srid testval) (neo4cl::srid result)))
        (fiveam:is (equalp (neo4cl::x testval) (neo4cl::x result)))
        (fiveam:is (equalp (neo4cl::y testval) (neo4cl::y result)))
        (fiveam:is (equalp (neo4cl::z testval) (neo4cl::z result)))))
    (neo4cl:bolt-transaction-autocommit session "MATCH (p:foo) DELETE p")
    ;; Disconnect
    (neo4cl::log-message :debug "Disconnecting session.")
    (neo4cl:disconnect session)))
