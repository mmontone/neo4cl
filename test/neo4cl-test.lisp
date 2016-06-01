(in-package #:neo4cl-test)

(defparameter *server*
  ;; We don't need to specify hostname, port or protocol, since in this
  ;; case we're using the defaults.
  ;; Should probably test each of the permutations, though.
  (make-instance 'neo4cl:neo4j-rest-server
                 :dbpasswd "wallaby"))


;;;; The actual test suite
(fiveam:def-suite neo4cl)
(fiveam:in-suite neo4cl)

(fiveam:test
  neo4j-base-api
  "Test the lowest-level methods for interacting with Neo4j"
  ;; Can we authenticate?
  (multiple-value-bind (body numeric verbal headers)
    (neo4cl::get-user-status *server*)
    (declare (ignore body)
             (ignore headers))
    (fiveam:is (equal numeric 200))
    (fiveam:is (equal verbal "OK")))
  ;; method: discover-rest-api
  (let ((result (neo4cl::discover-rest-api *server*)))
    (fiveam:is (listp result))
    (fiveam:is (equalp (car result) '(:extensions))))
  ;; Store a node
  (fiveam:is (listp (neo4cl:neo4j-transaction
                      *server*
                      `((:STATEMENTS
                          ((:STATEMENT . "CREATE (n:Person { name : {name} }) RETURN n")
                           (:PARAMETERS .
                                        ((:properties .
                                                      ((:name . "Andre")))))))))))
  ;; Retrieve a node
  (fiveam:is (equal
               (car
                 (second (assoc :data
                                (neo4cl:neo4j-transaction
                                  *server*
                                  `((:STATEMENTS
                                      ((:STATEMENT . "MATCH (x:Person {name: 'Andre'}) RETURN x.name")
                                       (:PARAMETERS .
                                                    ((:properties .
                                                                  ((:name . "Andre"))))))))))))
               "Andre"))
  ;; Delete a node
  (let ((result (neo4cl:neo4j-transaction
                  *server*
                  `((:STATEMENTS
                      ((:STATEMENT . "MATCH (x:Person {name: 'Andre'}) RETURN x.name")
                       (:PARAMETERS .
                                    ((:properties .
                                                  ((:name . "Andre")))))))))))
    (fiveam:is (listp result))
    (fiveam:is (equal (car (first result)) :columns))
    (fiveam:is (equal (car (second result)) :data))))
