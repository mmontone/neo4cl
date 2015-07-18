;;;; Classes and methods specific to neo4j

(in-package #:neo4cl)


;;; Classes

(defclass neo4j-rest-server ()
  ((protocol :initarg :protocol
             :initform "http"
             :reader protocol
             :documentation "The protocol by which we should connect to the Neo4J server. Default is 'http', so you should only need to set this explicitly if you're using HTTPS.")
   (hostname :initarg :hostname
             :initform "localhost"
             :reader hostname
             :documentation "The hostname to use when connecting to the Neo4J server. Defaults to 'localhost'.")
   (port :initarg :port
         :initform 7474
         :reader port
         :documentation "The port on which we connect to the Neo4J server. Defaults to 7474.")
   (dbuser :initarg :dbuser
           :initform (error "dbuser is a required initarg.")
           :reader dbuser
           :documentation "Username with which we authenticate to the database.")
   (dbpasswd :initarg :dbpasswd
             :initform (error "dbpasswd is a required initarg.")
             :reader dbpasswd
             :documentation "Password with which we authenticate to the database."))
  (:documentation "Neo4j REST API endpoint. Defines all the attributes needed for connecting to it."))


(defmethod base-url ((server neo4j-rest-server))
  "Extract the base-url of a Neo4J server, from a neo4j-rest-server object."
  (concatenate 'string (protocol server) "://" (hostname server) ":" (princ-to-string (port server))))


;;; Utilities

(defun get-user-status (server)
  "Authenticate to the Neo4J server and confirm the status of the database user.
200/OK means the credentials are good.
401/Unauthorized means they're not.
The presence of (:PASSWORD--CHANGE--REQUIRED . T) in the body of the reply, well, you get the hint."
  (neo4j-cypher-get-request server (concatenate 'string "/user/" (dbuser server))))

(defun change-password (server new-password)
  "Change a Neo4J user's password.
If the db operation is successful, update the server object so that this takes immediate effect within this server."
  (multiple-value-bind (body numeric verbal headers)
  (neo4j-cypher-post-request server
                             `((:password . ,new-password))
                             (concatenate 'string "/user/" (dbuser server) "/password"))
    (declare (ignore body)
             (ignore headers))
    (when (and (equal numeric 200)
               (equal verbal "OK"))
      (setf (slot-value server 'dbpasswd) new-password))))

(defun decode-neo4j-json (json)
  "Parse the JSON returned by Neo4J into a CL structure"
  ;; Neo4j sends a stream of octets. Convert this into a string.
  (let ((json-string (babel:octets-to-string json)))
    ;; If an empty string was returned, pass an empty string back.
    (if (equal json-string "")
        ""
        ;; If we received actual content, on the other hand, decode it.
        (cl-json:decode-json-from-string json-string))))

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

(define-condition neo4j-transaction-error (error)
  ((code :initarg :code :reader code)
   (message :initarg :message :reader message)))

(define-condition neo4j-integrity-error (neo4j-transaction-error)
  ())

(defun neo4j-transaction (endpoint statements)
  "Execute one or more Cypher statements, wrapped in a transaction.
   For now, we're simply issuing all statements in a batch and then committing, instead of building it up over several HTTP requests.
   Sample input:
   '((:statements
       ((:statement . \"MATCH (n:User {properties} ) RETURN n\")
        (:parameters (:properties (:name . \"bob\"))))))"
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
                         :content (cl-json:encode-json-alist-to-string statements))
    (declare (ignore headers)
             (ignore uri)
             (ignore stream)
             (ignore ignore))
    (let* ((response (decode-neo4j-json reply-content))
           (errors (second (second response))))
      (if
        ;; If an error was returned, throw it
        errors
        (error
          ;; Schema violation.
          ;; Usually because we attempted to create a user that already exists
          (if (equal (cdr (assoc :code errors)) "Neo.ClientError.Schema.ConstraintViolation")
            'neo4j-integrity-error
            'neo4j-transaction-error)
          :code (cdr (assoc :code errors))
          :message (cdr (assoc :message errors)))

        ;; If everything's OK, return the values we received along with the status codes
        (values response code reason)))))

(defmethod discover-rest-api ((endpoint neo4j-rest-server))
  (neo4j-cypher-get-request endpoint "/db/data/"))
