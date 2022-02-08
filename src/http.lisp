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


;;;; HTTP driver for Neo4j
;;;;
;;;; Warning: this thing is primitive. It works, but lacks refinements such as escaping control characters.

(in-package #:neo4cl)


;;; Classes

(defclass neo4j-rest-server ()
  ((protocol :initarg :protocol
             :initform "http"
             :reader protocol
             :documentation "String; the protocol by which we should connect to the Neo4J server. Default is 'http', so you should only need to set this explicitly if you're using HTTPS.")
   (hostname :initarg :hostname
             :initform "localhost"
             :reader hostname
             :documentation "String; the hostname to use when connecting to the Neo4J server. Defaults to 'localhost'.")
   (dbname :initarg :dbname
           :initform "neo4j"
           :reader dbname
           :documentation "String; the name of the database to query. Defaults to 'neo4j'.")
   (port :initarg :port
         :initform 7474
         :reader port
         :documentation "Integer; the port on which we connect to the Neo4J server. Defaults to 7474.")
   (dbuser :initarg :dbuser
           :initform "neo4j"
           :reader dbuser
           :documentation "Username with which we authenticate to the database.")
   (dbpasswd :initarg :dbpasswd
             :initform "neo4j"
             :reader dbpasswd
             :documentation "Password with which we authenticate to the database."))
  (:documentation "Neo4j REST API endpoint. Defines all the attributes needed for connecting to it."))


(defgeneric base-url (server)
  (:documentation "Extract the base-url of a Neo4J server, from a neo4j-rest-server object."))

(defmethod base-url ((server neo4j-rest-server))
  (concatenate 'string (protocol server) "://" (hostname server) ":" (princ-to-string (port server))))


;;; Utilities

(defun decode-neo4j-json (json)
  "Parse the JSON returned by Neo4J into a CL structure"
  ;; Neo4j sends a stream of octets. Convert this into a string.
  (let ((json-string (flexi-streams:octets-to-string json :external-format :UTF-8)))
    (log-message :debug (format nil "decode-neo4j-json decoding string '~A'" json-string))
    ;; If an empty string was returned, pass an empty string back.
    (if (equal json-string "")
        ""
        ;; If we received actual content, on the other hand, decode it.
        (cl-json:decode-json-from-string json-string))))

(defun extract-data-from-get-request (response)
  "Reach into the structure returned by a request that returns data,
   and return only the actual content."
  ;; Extract the data portion of the response
  (let ((data (cdr
                (assoc :data
                       (car
                         ;; Just the results section
                         (cdr (assoc :results
                                     ;; The actual query
                                     response)))))))
    ;; If there actually is data to be inspected, extract just the row portion.
    ;; If there isn't, return NIL.
    (when data
      (car
        (cdr
          (assoc :row
                 (car data)))))))

(defun extract-rows-from-get-request (response)
  "Reach into the structure returned by a request that returns data,
   and return only the actual content, in the form of a list of lists, each of
   the latter of which represents a row."
  (let ((data (cdr
                (assoc :data
                       (car
                         ;; Just the results section
                         (cdr (assoc :results response)))))))
    (when data
      (mapcar #'(lambda (datum)
                  (cdr (assoc :row datum)))
              data))))

(defun neo4j-transaction (endpoint statements)
  "Execute one or more Cypher statements, wrapped in a transaction.
   For now, we're simply issuing all statements in a batch and then committing,
   instead of building it up over several HTTP requests.
   Sample input:
   '((:statements
       ((:statement . \"MATCH (n:User {properties} ) RETURN n\")
        (:parameters (:properties (:name . \"bob\"))))))
   If an error is detected, it will be signalled with whatever information was provided by Neo4J"
  (handler-case
    (multiple-value-bind (reply-content code headers uri stream ignore reason)
      (let ((content (cl-json:encode-json-alist-to-string statements)))
        (log-message :debug (format nil "neo4j-transaction preparing to apply content '~A'" content))
        (drakma:http-request (concatenate 'string (base-url endpoint) "/db/" (dbname endpoint) "/tx/commit")
                             :method :post
                             :accept "application/json; charset=UTF-8"
                             :content-type "application/json"
                             :basic-authorization `(,(dbuser endpoint) ,(dbpasswd endpoint))
                             :content content))
      ;; We only bound these values to make m-v-b work properly.
      (declare (ignore headers)
               (ignore uri)
               (ignore stream)
               (ignore ignore))
      ;; Prepare to bind some common restart cases
      (restart-case
        ;; Process the response we got, returning either the content or an error
        (let* ((response (decode-neo4j-json reply-content))
               (errors (second (second response))))
          (log-message
            :debug
            (format nil "~&neo4j-transaction received response '~A' after decoding" response))
          ;; If an error was returned, throw it
          (if errors
              (let* ((error-code (cdr (assoc :code errors)))
                     (error-components (cl-ppcre:split "\\." error-code))
                     (classification (second error-components))
                     (category (third error-components))
                     (title (fourth error-components)))
                (cond
                  ;; The Client sent a bad request - changing the request might yield a successful outcome.
                  ((equal classification "ClientError")
                   (error 'client-error :category category :title title :message (cdr (assoc :message errors))))
                  ;; There are notifications about the request sent by the client.
                  ((equal classification "ClientNotification")
                   (warn 'client-notification :category category :title title :message (cdr (assoc :message errors))))
                  ;; The database cannot service the request right now, retrying later might yield a successful outcome.
                  ((equal classification "TransientError")
                   (error 'transient-error :category category :title title :message (cdr (assoc :message errors))))
                  ;; The database failed to service the request.
                  ((equal classification "DatabaseError")
                   (error 'database-error :category category :title title :message (cdr (assoc :message errors))))
                  ;; Anything else.
                  (t (error "Unknown error occurred: ~A" error-code))))
              ;; If everything's OK, return the values we received along with the status codes
              (values response code reason)))
        ;;; Restart cases start here
        ;; The ultimate cop-out: FIDO
        (return-nil () nil)))
    ;; The server is not listening on this socket
    (USOCKET:CONNECTION-REFUSED-ERROR
      (e)
      (declare (ignore e))
      (error 'service-error
             :category "server"
             :message "Connection refused"))))
