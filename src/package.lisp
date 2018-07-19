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
    ;; Data structures
    neo4j-rest-server
    ;; Functions
           extract-data-from-get-request
           extract-rows-from-get-request
           change-password
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
           message
           ))
