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


(defclass unbound-relationship ()
  ((id :initarg :id
       :reader id
       :type integer)
   (relationship-type :initarg :relationship-type
                      :reader relationship-type
                      :type string
                      :documentation "This is just called `type` in the spec, but this conflicts with a reserved word in CL.")
   (properties :initarg :properties
               :reader properties
               :type hash-table
               :documentation "The documented type is Dictionary, so it's represented here as a hash-table."))
  (:documentation "A relationship without information of start and end node id. It is used internally for Path serialization."))

(defclass path ()
  ((nodes :initarg :nodes
          :reader nodes
          :type list
          :documentation "List of `node` objects.")
   (rels :initarg :rels
         :reader rels
         :type list
         :documentation "List of unbound-relationship objects.")
   (indices :initarg :indices
            :reader indices
            :type list
            :documentation "List of integers describing how to construct the path from nodes and rels."))
  (:documentation "Represents a path between nodes."))

(defclass date ()
  ((days :initarg :days
         :reader days
         :type integer
         :documentation "Days since the Unix epoch"))
  (:documentation "An instant capturing the date, but not the time, nor the time zone."))

(defclass timestructure ()
  ((nanoseconds :initarg :nanoseconds
                :reader nanoseconds
                :type integer
                :documentation "Nanoseconds since midnight. This time is not UTC.")
   (tz-offset-seconds :initarg :tz-offset-seconds
                      :reader tz-offset-seconds
                      :type integer
                      :documentation "Offset in seconds from UTC."))
  (:documentation "An instant capturing the time of day, and the timezone, but not the date. Named `time` in the spec, but this conflicts with a reserved word in CL."))

(defclass localtime ()
  ((nanoseconds :initarg :nanoseconds
                :reader nanoseconds
                :type integer
                :documentation "Nanoseconds since midnight."))
  (:documentation "An instant capturing the time of day, but not the date, nor the time zone."))

(defclass datetime ()
  ((seconds :initarg :seconds
            :reader seconds
            :type integer
            :documentation "Seconds since the adjusted Unix epoch. This is not UTC.")
   (nanoseconds :initarg :nanoseconds
                :reader nanoseconds
                :type integer)
   (tz-offset-seconds :initarg :tz-offset-seconds
                      :reader tz-offset-seconds
                      :type integer
                      :documentation "Offset in seconds from UTC."))
  (:documentation "An instant capturing the date, the time, and the time zone. The time zone information is specified with a zone offset."))

(defclass datetimezoneid ()
  ((seconds :initarg :seconds
            :reader seconds
            :type integer
            :documentation "Seconds since the adjusted Unix epoch. This is not UTC.")
   (nanoseconds :initarg :nanoseconds
                :reader nanoseconds
                :type integer)
   (tz-id :initarg :tz-id
                      :reader tz-id
                      :type string
                      :documentation "Identifier for a specific time zone, such as 'Europe/Paris'."))
  (:documentation "An instant capturing the date, the time, and the time zone. The time zone information is specified with a zone identifier."))

(defclass localdatetime ()
  ((seconds :initarg :seconds
            :reader seconds
            :type integer
            :documentation "Seconds since the Unix epoch.")
   (nanoseconds :initarg :nanoseconds
                :reader nanoseconds
                :type integer))
  (:documentation "An instant capturing the date and the time, but not the time zone."))

(defclass duration ()
  ((months :initarg :months
           :reader months
           :type integer)
   (days :initarg :days
         :reader days
         :type integer)
   (seconds :initarg :seconds
            :reader seconds
            :type integer)
   (nanoseconds :initarg :nanoseconds
                :reader nanoseconds
                :type integer))
  (:documentation "A temporal amount. This captures the difference in time between two instants. Can be negative."))

(defclass point2d ()
  ((srid :initarg :srid
         :reader srid
         :type integer
         :documentation "A Spatial Reference System IDentifier.")
   (x :initarg :x
      :reader x
      :type float)
   (y :initarg :y
      :reader y
      :type float))
  (:documentation "Represents a single location in 2-dimensional space."))

(defclass point3d ()
  ((srid :initarg :srid
         :reader srid
         :type integer
         :documentation "A Spatial Reference System IDentifier.")
   (x :initarg :x
      :reader x
      :type float)
   (y :initarg :y
      :reader y
      :type float)
   (z :initarg :z
      :reader z
      :type float))
  (:documentation "Represents a single location in 3-dimensional space."))
