;   Copyright 2017-2022 James Fleming <james@electronic-quill.net>
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

(asdf:defsystem #:neo4cl
  :serial t
  :license "MIT license"
  :author "James Fleming <james@electronic-quill.net>"
  :description "Basic library for interacting with Neo4J"
  :depends-on (#:cl-base64      ; base64 encoding/decoding
               #:cl-ppcre       ; Check UUID formatting and identify error strings
               #:cl-json        ; Encode/decode json requests
               #:drakma         ; Send requests to the neo4j server
               #:flexi-streams  ; Convert between strings and octets
               #:usocket        ; Universal sockets library
               #:trivial-utf-8  ; Efficient UTF-8 serialisation/deserialisation
               )
  :components ((:file "package")
               (:file "logging")
               (:file "conditions")
               (:file "http")
               (:file "packstream-structures")
               (:file "packstream-encoding")
               (:file "packstream-decoding")
               (:file "bolt")))
