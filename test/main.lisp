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


(in-package #:neo4cl-test)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))

(defparameter *bolt-auth-basic*
  (make-instance 'neo4cl:bolt-auth-basic
                 :username "neo4j"
                 :password "wallaby"))

(defparameter *bolt-auth-none*
  (make-instance 'neo4cl:bolt-auth-none
                 :username "neo4j"))

(defparameter *bolt-server*
  (make-instance 'neo4cl:bolt-server
                 :hostname "192.0.2.1"
                 :auth-token *bolt-auth-basic*))


;;;; The parent test suite
(fiveam:def-suite main)
(fiveam:in-suite main)
