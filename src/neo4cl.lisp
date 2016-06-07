;;;; Classes and methods specific to neo4j

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
  (let ((json-string (flexi-streams:octets-to-string json)))
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


;;; Deprecated methods

(defun neo4j-cypher-get-request (endpoint uri)
  "Send a GET request to the legacy Cypher API, and return the parsed result.
   DEPRECATED: use neo4j-transaction instead, unless you have a specific reason not to."
  (multiple-value-bind (content code headers uri stream ignore reason)
    (drakma:http-request
      (concatenate 'string (base-url endpoint) uri)
      :accept "application/json; charset=UTF-8"
      :content-type "application/json"
      :additional-headers `(("authorization"
                             ,(concatenate 'string "Basic "
                                           (cl-base64:string-to-base64-string
                                             (format nil "~A:~A"
                                                     (dbuser endpoint)
                                                     (dbpasswd endpoint)))))))
    (declare (ignore uri)
             (ignore stream)
             (ignore ignore))
    (values (decode-neo4j-json content) code reason headers)))

(defmethod discover-rest-api ((endpoint neo4j-rest-server))
  (neo4j-cypher-get-request endpoint "/db/data/"))

(defun get-user-status (server)
  "Authenticate to the Neo4J server and confirm the status of the database user.
   200/OK means the credentials are good.
   401/Unauthorized means they're not.
   The presence of (:PASSWORD--CHANGE--REQUIRED . T) in the body of the reply, well, you get the hint."
  (neo4j-cypher-get-request server (concatenate 'string "/user/" (dbuser server))))

(defun neo4j-cypher-post-request (endpoint content &optional uri)
  "Store something in Neo4j via the legagy Cypher API.
   Expects content in the form of a list of lists, as input to cl-json:encode-json-alist-to-string.
   DEPRECATED: use neo4j-transaction instead, unless you have a specific reason not to."
  (multiple-value-bind (reply-content code headers uri stream ignore reason)
    (drakma:http-request (concatenate 'string (base-url endpoint) (or uri "/db/data/cypher"))
                         :method :post
                         :accept "application/json; charset=UTF-8"
                         :content-type "application/json"
                         :content (cl-json:encode-json-alist-to-string content)
                         :additional-headers `(("authorization"
                                                ,(concatenate 'string "Basic "
                                                              (cl-base64:string-to-base64-string
                                                                (format nil "~A:~A"
                                                                        (dbuser endpoint)
                                                                        (dbpasswd endpoint)))))))
    (declare (ignore headers)
             (ignore uri)
             (ignore stream)
             (ignore ignore))
    (values (decode-neo4j-json reply-content) code reason)))

(defun change-password (server new-password)
  "Change a Neo4J user's password.
   If the db operation is successful, update the server object so that this takes immediate effect within this server.
   NB: remember to update whatever you're using to initialise your db connection."
  (multiple-value-bind (body numeric verbal headers)
    (neo4j-cypher-post-request server
                               `((:password . ,new-password))
                               (concatenate 'string "/user/" (dbuser server) "/password"))
    (declare (ignore body)
             (ignore headers))
    (when (and (equal numeric 200)
               (equal verbal "OK"))
      (setf (slot-value server 'dbpasswd) new-password))))


;;; The modern interface

;; Classes of Neo4J status codes
;; Based on the documentation at http://neo4j.com/docs/developer-manual/current/#status-codes

;; The Client sent a bad request - changing the request might yield a successful outcome.
;; Effect on transaction: rollback
(define-condition client-error (error)
  ((category :initarg :category :reader category)
   (title :initarg :title :reader title)
   (message :initarg :message :reader message)))

;; There are notifications about the request sent by the client.
;; Effect on transaction: none
(define-condition client-notification ()
  ((category :initarg :category :reader category)
   (title :initarg :title :reader title)
   (message :initarg :message :reader message)))

;; The database cannot service the request right now, retrying later might yield a successful outcome.
;; Effect on transaction: rollback
(define-condition transient-error (error)
  ((category :initarg :category :reader category)
   (title :initarg :title :reader title)
   (message :initarg :message :reader message)))

;; The database failed to service the request.
;; Effect on transaction: rollback
(define-condition database-error (error)
  ((category :initarg :category :reader category)
   (title :initarg :title :reader title)
   (message :initarg :message :reader message)))


(defun neo4j-transaction (endpoint statements)
  "Execute one or more Cypher statements, wrapped in a transaction.
   For now, we're simply issuing all statements in a batch and then committing, instead of building it up over several HTTP requests.
   Sample input:
   '((:statements
       ((:statement . \"MATCH (n:User {properties} ) RETURN n\")
        (:parameters (:properties (:name . \"bob\"))))))
   If an error is detected, it will be signalled with whatever information was provided by Neo4J"
  (multiple-value-bind (reply-content code headers uri stream ignore reason)
    (drakma:http-request (concatenate 'string (base-url endpoint) "/db/data/transaction/commit")
                         :method :post
                         :accept "application/json; charset=UTF-8"
                         :content-type "application/json"
                         :additional-headers `(("authorization"
                                                ,(concatenate 'string "Basic "
                                                              (cl-base64:string-to-base64-string
                                                                (format nil "~A:~A"
                                                                        (dbuser endpoint)
                                                                        (dbpasswd endpoint))))))
                         :content (cl-json:encode-json-alist-to-string
                                    statements))
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
      (return-nil () nil)
      ;; Just tell us what went wrong
      (report-error (e) (format nil "Error code: '~A - ~A' Error message: '~A'" (category e) (title e) (message e))))))


