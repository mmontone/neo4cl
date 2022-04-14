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
    ;; Main classes and their methods
    bolt-server
    bolt-auth-basic
    bolt-auth-none
    neo4j-version
    neo4j-4-3-9
    hostname
    port
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
    unbound-relationship
    path
    date
    timestructure
    localtime
    datetime
    datetimezoneid
    localdatetime
    duration
    point2d
    point3d
    ;; Conditions
    bolt-error
    client-error
    database-error
    packstream-error
    service-error
    transient-error
    category
    message
    title))
