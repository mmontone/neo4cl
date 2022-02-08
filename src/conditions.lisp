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


;;;; Condition objects, for signalling errors, etc.

(in-package #:neo4cl)


;; Classes of Neo4J status codes
;; Based on the documentation at http://neo4j.com/docs/developer-manual/current/#status-codes

(define-condition client-error (error)
  ((category :initarg :category :reader category)
   (title :initarg :title :reader title)
   (message :initarg :message :reader message))
  (:documentation "The Client sent a bad request - changing the request might yield a successful outcome. Effect on transaction: rollback."))

(define-condition client-notification ()
  ((category :initarg :category :reader category)
   (title :initarg :title :reader title)
   (message :initarg :message :reader message))
   (:documentation "There are notifications about the request sent by the client. Effect on transaction: none."))

(define-condition transient-error (error)
  ((category :initarg :category :reader category)
   (title :initarg :title :reader title)
   (message :initarg :message :reader message))
  (:documentation "The database cannot service the request right now, retrying later might yield a successful outcome. Effect on transaction: rollback."))

(define-condition database-error (error)
  ((category :initarg :category :reader category)
   (title :initarg :title :reader title)
   (message :initarg :message :reader message))
  (:documentation "The database failed to service the request. Effect on transaction: rollback."))


;; Conditions relating to the connection itself

(define-condition service-error (error)
  ((category :initarg :category :reader category)
   (message :initarg :message :reader message))
  (:documentation "Conditions _not_ reported from Neo4J, like connection refused"))

(define-condition bolt-error (error)
  ((category :initarg :category :reader category)
   (message :initarg :message :reader message))
  (:documentation "The server reported an error relating to a Bolt session."))

(define-condition packstream-error (error)
  ((category :initarg :category :reader category)
   (message :initarg :message :reader message))
  (:documentation "Some kind of issue relating to Packstream serialisation/deserialisation."))
