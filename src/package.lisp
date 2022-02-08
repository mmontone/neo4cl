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

(defpackage neo4cl
  (:use
    #:cl)
  (:export
    ;;; Bolt client
    ;; Classes
    bolt-server
    bolt-auth-basic
    bolt-auth-none
    neo4j-version
    neo4j-4-3-9
    ;; bolt-session class and its methods
    bolt-session
    connect
    connection
    connection-id
    bolt-version
    neo4j-version
    establish-bolt-session
    bolt-transaction-autocommit
    disconnect
    ;; bolt-protocol class, subclasses and methods
    bolt-protocol
    bolt-protocol-4-3
    major-version
    minor-version
    ;; Structure types and their methods
    node
    node-id
    node-labels
    node-properties
    relationship
    relationship-id
    start-node-id
    end-node-id
    relationship-type
    relationship-properties
    ;; Conditions
    bolt-error
    packstream-error
    ;;;
    ;;; HTTP client
    ;; Data structures
    neo4j-rest-server
    dbname
    dbpasswd
    hostname
    port
    ;; Functions
    change-password
    extract-data-from-get-request
    extract-rows-from-get-request
    get-user-status
    neo4j-transaction
    ;; Errors and sundry conditions
    client-error
    client-notification
    transient-error
    database-error
    service-error
    ;; Error-related symbols
    category
    title
    message))
