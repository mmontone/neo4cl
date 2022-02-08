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


;;;; Structures and functions for connecting to Neo4j via the Bolt protocol

(in-package #:neo4cl)


;;;; For the API spec:
;;;; https://7687.org/driver_api/driver-api-specification.html


;;;; Constants

;; Protocol stuff
;;
;; Yes, I'm using `defparameter` and constant-style naming.
;; They _were_ constants, but using defconstant with non-atomic values produces a compile-time error:
;; http://www.sbcl.org/manual/index.html#Defining-Constants
;; So I changed the style of definition, but not the naming.
(defparameter +bolt-announce-protocol+ (vector #x60 #x60 #xB0 #x17))
(defparameter +bolt-termination-chunk+ (vector #x00 #x00))

;;; Message types
;; Request messages
(defconstant +bolt-hello+ #x01)
(defconstant +bolt-goodbye+ #x02)
(defconstant +bolt-reset+ #x0F)
(defconstant +bolt-run+ #x10)
(defconstant +bolt-discard+ #x2F)
(defconstant +bolt-pull+ #x3F)
(defconstant +bolt-begin+ #x11)
(defconstant +bolt-commit+ #x12)
(defconstant +bolt-rollback+ #x13)
(defconstant +bolt-route+ #x66)
;; Summary messages
(defconstant +bolt-success+ #x70)
(defconstant +bolt-ignored+ #x7E)
(defconstant +bolt-failure+ #x7F)
;; Detail messages
(defconstant +bolt-record+ #x71)


;;;; Classes

(defclass bolt-auth-token ()
  ((scheme :reader scheme
           :type string
           :initform "undefined"
           :allocation :class))
  (:documentation "Parent class for authentication tokens, to be subclassed for each auth scheme."))

(defclass bolt-auth-none (bolt-auth-token)
  ((scheme :initform "none"
           :documentation "Indicates to the server that the 'none' authentication scheme is requested.")
   (username :initarg :username
             :reader username
             :type string
             :initform "neo4j"
             :documentation "The username with which we're authenticating to the Neo4j server. Default is neo4j."))
  (:documentation "Authentication token for the 'none' authentication scheme."))

(defclass bolt-auth-basic (bolt-auth-token)
  ((scheme :initform "basic"
           :documentation "Indicates to the server that the 'basic' authentication scheme is expected.")
   (username :initarg :username
             :reader username
             :type string
             :initform "neo4j"
             :documentation "The username with which we're authenticating to the Neo4j server. Default is neo4j.")
   (password :initarg :password
             :reader password
             :type string
             :initform "neo4j"
             :documentation "The password with which we're authenticating to the Neo4j server. Default is neo4j."))
  (:documentation "Authentication token for basic auth."))

(defclass bolt-server ()
  ((hostname :initarg :hostname
             :reader hostname
             :type string
             :initform "localhost"
             :documentation "The host on which Neo4j is listening. Default is localhost.")
   (port :initarg :port
         :reader port
         :type integer
         :initform 7687
         :documentation "The port-number on which Neo4j is listening. Default is 7687.")
   (encryption :initarg :encrypted
               :reader encrypted
               :type boolean
               :initform nil
               :documentation "Whether the Neo4j server is using TLS encryption.")
   (timeout :initarg :timeout
            :reader timeout
            :type integer
            :initform 20
            :documentation "The timeout on creating connections, in seconds.")
   (auth-token :initarg :auth-token
               :reader auth-token
               :type bolt-auth-token
               :initform (make-instance 'bolt-auth-none :username "neo4j")
               :documentation "Authentication credentials."))
  (:documentation "A Neo4j speaker, using the Bolt protocol."))

(defclass bolt-protocol ()
  ((major-version :reader major-version
                  :type integer
                  :initform 0
                  :allocation :class)
   (minor-version :reader minor-version
                  :type integer
                  :initform 0
                  :allocation :class))
  (:documentation "Represents a version of the Bolt protocol. Subclasses are used for specialising methods according to the protocol version.
                   For more detail about the protocol itself, see https://7687.org/bolt/bolt-protocol-message-specification-4.html"))

(defclass bolt-protocol-4-3 (bolt-protocol)
  ((major-version :initform 4)
   (minor-version :initform 3))
  (:documentation "Bolt protocol version 4.3"))


(defclass neo4j-version ()
  ((major-version :reader major-version
                  :type integer
                  :initform 0
                  :allocation :class)
   (minor-version :reader minor-version
                  :type integer
                  :initform 0
                  :allocation :class)
   (patch-release :reader patch-release
                  :type integer
                  :initform 0
                  :allocation :class))
  (:documentation "Represents a version of the Neo4j database. Subclasses are used for specialising client methods according to the database version."))

(defclass neo4j-4-3-9 (neo4j-version)
  ((major-version :initform 4)
   (minor-version :initform 3)
   (patch-release :initform 9))
  (:documentation "Neo4j version 4.3.9"))


(defclass bolt-session ()
  ((connection :initarg :connection
               :reader connection
               :type usocket:stream-usocket
               :initform (error "connection attribute is mandatory.")
               :documentation "The network connection object for this session.")
   (connection-id :initarg :connection-id
                  :reader connection-id
                  :type string
                  :initform ""
                  :documentation "The connection ID supplied by the server in response to a Hello message.")
   (bolt-version :initarg :bolt-protocol-version
                 :reader bolt-protocol-version
                 :type bolt-protocol
                 :initform (error "bolt-protocol-version attribute is mandatory.")
                 :documentation "The version of the Bolt protocol in effect for this session.")
   (neo4j-version :initarg :neo4j-version
                  :accessor neo4j-version
                  :type neo4j-version
                  :initform (make-instance 'neo4j-version)
                  :documentation "The version of the Neo4j database to which this session is connected. This is determined in a separate operation after the `handshake` method creates and initialises this object. Initial value should not be valid for any client use."))
  (:documentation "Composite object containing everything you need to know about this session."))


;;; Methods

(defgeneric connect (server)
  (:documentation "Create a connection to a Neo4j server
                   Return a 2-element list:
                   - usocket:stream-socket instance containing the TCP connection.
                   - bolt-protocol instance indicating the version of the Bolt protocol to use in this session."))

(defmethod connect ((server bolt-server))
  (log-message :debug (format nil "Connecting to Bolt server at ~A:~A"
                              (hostname server) (port server)))
  (let ((connection (usocket:socket-connect (hostname server)
                                            (port server)
                                            :protocol :stream   ; TCP, not UDP
                                            :element-type '(unsigned-byte 8)
                                            :timeout (timeout server))))
    ;;; Handshake reference docs:
    ;;; https://7687.org/bolt/bolt-protocol-handshake-specification.html
    ;;
    ;; Identify that this is a Bolt connection.
    ;; No response is expected.
    (log-message :debug "Handshake: sending Bolt protocol ID.")
    (write-sequence +bolt-announce-protocol+ (usocket:socket-stream connection))
    (force-output (usocket:socket-stream connection))
    ;; Negotiate the protocol version.
    ;; "In this, the client submits exactly four protocol versions,
    ;;  each encoded as a big-endian 32-bit unsigned integer for a total of 128 bits."
    ;;
    ;; We only support version 4.3, so this should be a short discussion.
    ;;
    ;; Notes about the handshake in v4.3:
    ;; The first 8 bits are reserved.
    ;; The second octet represents the number of consecutive minor versions
    ;; below the current version that the client supports.
    ;; The third octet represents the minor version.
    ;; The fourth octet represents the major version.
    (log-message :debug "Handshake: negotiating Bolt protocol version.")
    (write-sequence (vector #x00 #x00 #x03 #x04   ; We speak version 4.3; no prior minor versions.
                            #x00 #x00 #x00 #x00   ; We speak no other versions.
                            #x00 #x00 #x00 #x00   ; We speak no other versions.
                            #x00 #x00 #x00 #x00)  ; We speak no other versions.
                    (usocket:socket-stream connection))
    (force-output (usocket:socket-stream connection))
    ;;
    ;; Now the server should respond with its selected version.
    (log-message :debug "Handshake: reading server response.")
    ;; Create an array as an interim store for each of the returned elements
    (let ((server-version (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)))
      (setf (aref server-version 0) (read-byte (usocket:socket-stream connection)))
      (setf (aref server-version 1) (read-byte (usocket:socket-stream connection)))
      (setf (aref server-version 2) (read-byte (usocket:socket-stream connection)))
      (setf (aref server-version 3) (read-byte (usocket:socket-stream connection)))
      ;; From that array, create a protocol-version object
      (cond
        ;; Bolt version 4.3
        ((and (equal 4 (aref server-version 3))
              (equal 3 (aref server-version 2)))
         (let ((protocol-version (make-instance 'bolt-protocol-4-3)))
           (log-message :debug (format nil "Received server version '~A.~A'"
                                       (major-version protocol-version)
                                       (minor-version protocol-version)))
           ;; Return the usocket and protocol objects
           (list connection protocol-version)))
        ;; Any other version
        (t (error 'service-error
                  :category "protocol"
                  :message (format nil "Unrecognised protocol version ~A.~A"
                                   (aref server-version 3) (aref server-version 2))))))))


(defgeneric add-auth-to-extra (token hash)
  (:documentation "Add the details from this authentication token to that 'extras' hash-table."))

(defmethod add-auth-to-extra ((token bolt-auth-token) (hash hash-table))
  (setf (gethash "scheme" hash) (scheme token)))

(defmethod add-auth-to-extra ((token bolt-auth-basic) (hash hash-table))
  (setf (gethash "scheme" hash) (scheme token))
  (setf (gethash "principal" hash) (username token))
  (setf (gethash "credentials" hash) (password token)))


(defgeneric send-packstream (connection protocol data)
  (:documentation "Send Packstream data to a Bolt server."))

(defmethod send-packstream ((connection usocket:stream-usocket)
                            (protocol bolt-protocol)
                            (data list))
  "Accept data in the form of a list of vectors."
  (log-message :debug "Sending Packstream message to the Bolt server.")
  ;; Calculate the length of the message,
  ;; which we need to send as the chunk-header in front of the message itself.
  (let ((message-length (apply #'+ (mapcar #'length data))))
    ;(log-message :debug "Sending packstream length-header")
    ;; Convert the message-length into a 16-bit/2-octet vector for transmission
    (write-sequence (vector (ldb (byte 8 8) message-length)
                            (ldb (byte 8 0) message-length))
                    (usocket:socket-stream connection))
    ;(log-message :debug (format nil "Sending packstream data ~A" data))
    (mapcar #'(lambda (datum)
                (write-sequence datum (usocket:socket-stream connection)))
            data)
    ;(log-message :debug "Sending packstream termination chunk")
    (write-sequence +bolt-termination-chunk+ (usocket:socket-stream connection))
    (force-output (usocket:socket-stream connection))))


(defgeneric bolt-hello (connection protocol auth-token)
  (:documentation "Connect to the Neo4j server, authenticate, and ascertain the server version.
                   Return a bolt-session instance, which contains the connection, the bolt version and the Neo4j version."))

(defmethod bolt-hello ((connection usocket:stream-usocket)
                       (protocol bolt-protocol-4-3)
                       (auth-token bolt-auth-token))
  "Hello method specialised on Bolt v4.3.
   For details, see https://7687.org/bolt/bolt-protocol-message-specification-4.html#version-43"
  (log-message :debug "Sending HELLO message to the Bolt server")
  ;; Prepare a hash-table of headers.
  ;; The scope of this dict ends after we _send_ the Hello message.
  ;; FIXME: a hash-table is probably excessive. Can we get by with an alist?
  (let ((hello-dict (make-hash-table :test #'equal)))
    ;; Set the headers:
    ;;
    ;; User-agent string
    (setf (gethash "user_agent" hello-dict) "neo4cl/0.1.2")
    ;; Routing data: routing::Dictionary(address::String))
    ;; For now, explicitly set routing to Null (the server default)
    (setf (gethash "routing" hello-dict) nil)
    ;; Add the authentication scheme details.
    ;; The spec describes this as a String, but that's only part of the story,
    ;; hence dispatching this part out to a method that abstracts away the details.
    (add-auth-to-extra auth-token hello-dict)
    ;;
    ;; Send the request
    (send-packstream connection
                     protocol
                     (append  ; Assemble the message for transmission
                       (list (vector #xb1) ; Initial header: Structure, size 1.
                             (vector +bolt-hello+) ; HELLO code
                             (encode-hash-table hello-dict))))) ; "Extra" headers
  ;; Get the server response, which we *hope* will be "SUCCESS {}"
  (log-message :debug "Checking for response from the Neo4j server.")
  (let* ((response (parse-server-hello connection protocol))
         (neo4j-version (gethash "server" response))
         (connection-id  (gethash "connection_id" response))
         ;; We'll ignore the hints for now.
         (hints (gethash "hints" response)))
    (log-message :debug (format nil "Received HELLO response: '~A'" response))
    (log-message :debug (format nil "Ignoring hints for now. FYI, they're: ~A" hints))
    ;; Assemble and return a bolt-session instance
    (make-instance 'bolt-session
                   :connection connection
                   :connection-id connection-id
                   :bolt-protocol-version protocol
                   :neo4j-version
                   (cond
                     ((equal "Neo4j/4.3.9" neo4j-version)
                      (make-instance 'neo4j-4-3-9))
                     (t (error 'bolt-error
                               :category "bolt"
                               :message (format nil "Unhandled Neo4j version ~A" neo4j-version)))))))


(defgeneric read-bolt-message-from-stream (connection protocol &optional acc)
  (:documentation "Read a bolt message from the connection stream, and return a vector of octets for parsing.
  Expects to receive a stream, so if you're handling a usocket:stream-usocket object, you'll need to pass it the value of (usocket:socket-stream obj)."))

(defmethod read-bolt-message-from-stream ((connection stream)
                                          (protocol bolt-protocol)
                                          &optional acc)
  (let ((chunk (read-bolt-chunk-from-stream connection protocol)))
    (if chunk
        (read-bolt-message-from-stream connection protocol (push chunk acc))
        (vectorise-list (nreverse acc)))))


(defgeneric read-bolt-chunk-from-stream (connection protocol)
  (:documentation "Read a single Bolt chunk from the stream.
                   If length is >0, return a 1-dimention array of type (unsigned-byte 8).
                   If its length is 0, i.e. it's an end-of-message chunk, return nil."))

(defmethod read-bolt-chunk-from-stream ((connection stream)
                                        (protocol bolt-protocol))
  (log-message :debug (format nil "Attempting to read a Bolt chunk from the connection stream, using protocol version ~A.~A"
                              (major-version protocol) (minor-version protocol)))
  (let ((chunk-length 0))
    ;; First, get the chunk length, so we know how many octets to expect.
    (setf (ldb (byte 8 8) chunk-length) (read-byte connection))
    (setf (ldb (byte 8 0) chunk-length) (read-byte connection))
    ;(log-message :debug (format nil "Reported chunk length: ~D octets." chunk-length))
    ;; Only do all this processing if there's actual data to return
    (if (> chunk-length 0)
        ;; Now create a buffer vector of that length.
        (let ((buffer (make-array chunk-length :element-type '(unsigned-byte 8))))
          ;; Copy chunk-length octets into that vector,
          (loop for i from 0 to (- chunk-length 1)
                do (handler-case
                     (setf (aref buffer i) (read-byte connection))
                     (end-of-file ()
                                  (error 'bolt-error
                                         :category "chunk-parsing"
                                         :message (format nil "Unexpected EOM after ~D octets" i)))))
          ;; If we got this far, return the buffer for parsing.
          buffer)
        ;; If the reported chunk-length is 0, return an empty vector
        '())))


(defgeneric parse-server-hello (instream protocol-version)
  (:documentation "Process the server's response to a HELLO message.
                   More of a helper function, wrapped around parse-bolt-hello"))

(defmethod parse-server-hello ((instream usocket:stream-usocket)
                               (protocol-version bolt-protocol-4-3))
  (let* ((message (read-bolt-message-from-stream (usocket:socket-stream instream)
                                                 protocol-version))
         ;; Grab the message-type for ease of later reference.
         ;; Valid values are #x70 (success) and #x7F (failure).
         (message-type (aref message 1))
         ;; Both success and failure should return a dictionary.
         (details (decode-element message 2)))
    (cond
      ;; Success!
      ((equal +bolt-success+ message-type)
       (log-message :debug "Success! The server said hello back to us!")
       (log-message :debug (format nil "What we got: ~A" details))
       ;; Return the alistified Dictionary that the server returned
       ;; It should be an alist with keys "server", "connection_id" and "hints"
       details)
      ;; Failure!
      ((equal +bolt-failure+ message-type)
       (let ((error-code (gethash "code" details))
             (error-message (gethash "message" details)))
         (log-message :warn (format nil "Hello failure. Code: ~A; Message: ~A." error-code error-message))
         (error 'bolt-error
                :category error-code
                :message error-message)))
      ;; WTF!
      (t
       (error 'bolt-error
              :category "Unexpected response"
              :message (format nil "Unexpected response to HELLO: '~A'" message-type))))))


(defun read-tx-responses (session &key messages)
  "Digest the output of read-bolt-messages-from-stream (plural version), which should be a list of vectors.
  Return a list of lists, each representing a row of data, plus an alist of metadata.
  Expects its client function to have already sent a PULL message."
  (declare (type bolt-session session)
           (type (or null list) messages))
  (let ((message (read-bolt-message-from-stream
                   (usocket:socket-stream (connection session))
                   (bolt-protocol-version session))))
    ;; Determine whether it's a record message (#x71) or a summary message (most likely #x70).
    ;; This should be in the second octet;
    ;; the first should be #xb1, indicating a structure of one field.
    (if (equal +bolt-record+ (aref message 1))
      ;; It's a record.
      ;; Recurse, after parsing this message and prepending its contents to :messages
      (read-tx-responses
        session
        :messages (push (decode-list message 2) messages))
      ;; It's metadata.
      ;; Parse that, and return it as the second element in the results list.
      ;; The first element should be the messages, in order of arrival.
      ;; Parse the metadata
      (let ((metadata (decode-dictionary message 2)))
        ;; Record the message-type
        (setf (gethash "bolt-status" metadata)
              (if (equal +bolt-success+ (aref message 1))
                "success"
                "failure"))
        ;; Return it
        (list (nreverse messages) metadata)))))


(defgeneric parse-autocommit-tx-response (instream protocol-version)
  (:documentation "Process the server's response to a RUN message requesting an auto-commit transaction.
  On success, returns an alist with two elements:
  - fields (List<String>) = list of field names to expect in the return result.
  - t_first (Integer) = time in ms until the first record will be available."))

(defmethod parse-autocommit-tx-response ((instream usocket:stream-usocket)
                                         (protocol-version bolt-protocol-4-3))
  (log-message :debug "Attempting to parse the response to an autocommit RUN request.")
  (let* ((message (read-bolt-message-from-stream (usocket:socket-stream instream) protocol-version))
         ;; First octet should be a marker for a 1-entry structure.
         ;; Second should indicate success/failure/ignored
         (message-type (aref message 1)))
    ;; NB: only uncomment the following line if you *really* need a dump of the octets-vector
    ;;(log-message :debug (format nil "Received server message ~A" message))
    (cond
      ;; Success!
      ((equal +bolt-success+ message-type)
       (log-message :debug "TX response: SUCCESS")
       (if (> (length message) 0)
           (let ((result (decode-element message 2)))
             (log-message :debug (format nil "TX result: ~A" result))
             result)
           (log-message :debug "TX result was empty")))
      ;; Failure!
      ((equal +bolt-failure+ message-type)
       (log-message :debug "TX response: FAILURE")
       (let* ((details (decode-element message 2))
              (error-code (gethash "code" details))
              (error-message (gethash "message" details)))
         (log-message :warn (format nil "TX RUN failure. Code: ~A; Message: ~A." error-code error-message))
         (error 'bolt-error
                :category error-code
                :message error-message)))
      ;; Server don't care
      ((equal +bolt-ignored+ message-type)
       (log-message :warn "TX response: IGNORE")
       (error 'bolt-error
              :category "transaction"
              :message "Server returned IGNORE"))
      ;; EWTF
      (t
       (log-message :debug (format nil "Unexpected response to RUN: '~A'" message-type))
       (error 'bolt-error
              :category "Unexpected response"
              :message (format nil "Unexpected response to RUN: '~A'" message-type))))))


(defgeneric establish-bolt-session (server)
  (:documentation "Connect to a Neo4j server, take care of all the handshaking, and return a bolt-session instance, containing everything we need henceforth."))

(defmethod establish-bolt-session ((server bolt-server))
  ;; Initial handshake gets us a usocket stream connection, and the Bolt protocol-version in effect.
  (let ((initial-details (connect server)))
    ;; Now we perform a Hello
    (bolt-hello
      ;; TCP connection: usocket:stream-usocket instance
      (first initial-details)
      ;; Bolt protocol version: bolt-protocol instance
      (second initial-details)
      ;; Authentication token
      (auth-token server))))

(defun bolt-pull (session)
  "Send a PULL message to the server."
  (log-message :debug "Sending PULL message to the Bolt server")
  ;; Initialise the `extra` Dictionary for query parameters
  (let ((extra (make-hash-table :test #'equal)))
    ;; Request all the things
    (setf (gethash "n" extra) -1)
    (send-packstream
      (connection session)
      (bolt-protocol-version session)
      (list
        ;; Signature: this is a 1-element structure
        (vector #xb1)
        ; Bolt message ID
        (vector +bolt-pull+)
        ;; Parameters
        (encode-hash-table extra)))))


(defgeneric bolt-transaction-autocommit (session query &key parameters)
  (:documentation "Perform an autocommit transaction.
                   :parameters argument can be either an alist or a hash-table. alist is typically the low-overhead option, but it'll break if any of the values are null."))

(defmethod bolt-transaction-autocommit ((session bolt-session)
                                        (query string)
                                        &key
                                        (parameters '()))
  (declare (type (or list hash-table) parameters))
  (log-message :debug (format nil "Attempting to run autocommit transaction with query '~A'" query))
  ;; Initialise the `extra` dict
  (let ((extra (make-hash-table :test #'equal)))
    ;; Populate the `extra` dict
    ;(setf (gethash "bookmarks" extra) '())
    ;; Send RUN message
    (send-packstream
      (connection session)
      (bolt-protocol-version session)
      (list
        ;; Signature: this is a 1-element structure
        (vector #xb1)
        ; Bolt message ID
        (vector +bolt-run+)
        ;; The query itself
        (encode-string query)
        ;; Encode any supplied parameters
        (encode-element parameters)
        ;; Encode the extras
        (encode-hash-table extra))))
  ;; Receive response
  (log-message :debug "Attempting to parse server response to RUN message")
  (let ((tx-response-header
          (parse-autocommit-tx-response (connection session)
                                        (bolt-protocol-version session))))
    (if tx-response-header
        (let ((time-to-first-record (/ (gethash "t_first" tx-response-header) 1000))
              (fieldnames (gethash "fields" tx-response-header)))
          ;; Wait until the server says it'll be ready
          (log-message :debug (format nil "Sleeping ~D seconds, waiting for the server to be ready."
                                      time-to-first-record))
          (sleep time-to-first-record)
          ;; Send PULL request
          (bolt-pull session)
          (let ((records (read-tx-responses session)))
            (log-message :debug (format nil "Fetched record-set ~A" records))
            (log-message :debug "Converting records to alists")
            (mapcar #'(lambda (record)
                        (loop for i from 0 to (- (length record) 1)
                              collect (cons (nth i fieldnames)
                                            ;; The last field in the list is consed with nil,
                                            ;; because it's a proper list.
                                            ;; Catch this situation and extract it.
                                            (if (atom (nth i record))
                                                (nth i record)
                                                (car (nth i record))))))
                    (car records))))
        ;; Empty response from server
        (log-message :debug "Server response to RUN message was empty. Nothing to do here."))))


(defgeneric disconnect (session)
  (:documentation "Disconnect from the Neo4j server."))

(defmethod disconnect ((session bolt-session))
  (log-message :debug "Closing Bolt connection")
  ;; Politely hang up: GOODBYE
  (log-message :debug "Sending GOODBYE to the Neo4j server.")
  (write-sequence (vector +bolt-goodbye+) (usocket:socket-stream (connection session)))
  (force-output (usocket:socket-stream (connection session)))
  (usocket:socket-close (connection session)))
