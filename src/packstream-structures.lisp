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


;;;; Packstream structures
;;;; - Classes
;;;; - Constants
;;;;
;;;; "PackStream is a general purpose data serialisation format,
;;;;  originally inspired by (but incompatible with) MessagePack."
;;;; For more details, see https://7687.org/packstream/packstream-specification-1.html

(in-package #:neo4cl)


;;;; Constants

;; Markers
(defconstant +packstream-null+ #xC0)
(defconstant +packstream-false+ #xc2)
(defconstant +packstream-true+ #xc3)


;;;; Classes

(defclass node ()
  ((node-id :initarg :node-id
            :reader node-id
            :type integer
            :documentation "Node identifier. Mostly useful for correlating with relationships.")
   (node-labels :initarg :node-labels
                :reader :node-labels
                :type list
                :documentation "The node's labels, represented as a list of strings.")
   (node-properties :initarg :node-properties
                    :reader node-properties
                    :type hash-table
                    :documentation "The node's properties, as a hash-table."))
  (:documentation "Snapshot of a node within a graph database"))

(defclass relationship ()
  ((relationship-id :initarg :relationship-id
                    :reader relationship-id
                    :type integer
                    :documentation "ID for the relationship.")
   (start-node-id :initarg :start-node-id
                  :reader start-node-id
                  :type integer
                  :documentation "ID of the start-node for this relationship.")
   (end-node-id :initarg :end-node-id
                :reader end-node-id
                :type integer
                :documentation "ID of the start-node for this relationship.")
   (relationship-type :initarg :relationship-type
                      :reader relationship-type
                      :type string
                      :documentation "Neo4j type of this relationship.")
   (relationship-properties :initarg :relationship-properties
                            :reader relationship-properties
                            :type hash-table
                            :documentation "Properties recorded within this relationship"))
  (:documentation "Snapshot of a relationship within a graph database"))
