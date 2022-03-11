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


;;;; Test suite for the Packstream serialisation functions


(in-package #:neo4cl-test)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


(fiveam:def-suite packstream
                  :description "Tests for Packstream serialisation."
                  :in main)

(fiveam:in-suite packstream)


;;;; Pure functions

;; FIXME: Need to test strings >255 and >65535 in length.
(fiveam:test
  encode-string
  "Serialise strings into String data types."
  ;; Empty string
  (fiveam:is (equalp (vector #x80) (neo4cl::encode-string "")))
  ;; Small string: <16 bytes
  (fiveam:is (equalp (vector #x84 #x41 #x42 #x43 #x44)
                    (neo4cl::encode-string "ABCD")))
  ;; Longer string, smol edition
  (fiveam:is (equalp (vector #xD0 #x1A #x41 #x42 #x43 #x44 #x45 #x46 #x47 #x48 #x49 #x4A #x4B #x4C #x4D #x4E #x4F #x50 #x51 #x52 #x53 #x54 #x55 #x56 #x57 #x58 #x59 #x5A)
                    (neo4cl::encode-string "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))))

(fiveam:test
  encode-integer
  "Serialise integer values."
  (fiveam:signals (error "Integer out of range.") (neo4cl::encode-integer -9223372036854775809))
  (fiveam:signals (error "Integer out of range.") (neo4cl::encode-integer 9223372036854775808))
  ;; TinyInt
  (fiveam:is (equalp (vector #x00) (neo4cl::encode-integer 0)))
  (fiveam:is (equalp (vector #x2a) (neo4cl::encode-integer 42)))
  (fiveam:is (equalp (vector #xf2) (neo4cl::encode-integer -14)))
  ;; INT_8
  (fiveam:is (equalp (vector #xc9 #x02 #x9A) (neo4cl::encode-integer 666)))
  ;; INT_16
  (fiveam:is (equalp (vector #xca #x00 #x00 #xfa #x00) (neo4cl::encode-integer 64000)))
  ;; INT_32
  (fiveam:is (equalp (vector #xcb #x7f #xff #xff #xff #xff #xff #xff #xff)
                     (neo4cl::encode-integer 9223372036854775807)))
  ;; INT_64
  (fiveam:is (equalp (vector #xcb #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
                     (neo4cl::encode-integer -9223372036854775808))))

(fiveam:test
  encode-list
  "Serialise lists."
  ;; Null list
  (fiveam:is (equalp (vector #x90) (neo4cl::encode-list '())))
  ;; List with a single string element
  (fiveam:is (equalp (vector #x91 #x84 #x41 #x42 #x43 #x44)
                     (neo4cl::encode-list (list "ABCD"))))
  ;; List with a single integer element
  (fiveam:is (equalp (vector #x91 #x2a)
                     (neo4cl::encode-list (list 42))))
  ;; Heterogeneous list
  (fiveam:is (equalp (vector #x92 #x84 #x41 #x42 #x43 #x44 #x2a)
                     (neo4cl::encode-list (list "ABCD" 42)))))

(fiveam:test
  encode-hash-table
  "Serialise hash-tables."
  ;; Empty/null hash-table
  (fiveam:is (equalp (vector #xa0)
                     (neo4cl::encode-hash-table (make-hash-table :test #'equal))))
  ;; With contents
  (let ((hash (make-hash-table :test #'equal)))
    (setf (gethash "one" hash) "eins")
    (fiveam:is (equalp (vector #xA1
                               #x83 #x6F #x6E #x65
                               #x84 #x65 #x69 #x6E #x73)
                       (neo4cl::encode-hash-table hash)))))

(fiveam:test
  encode-alist
  "Serialise alists into Dictionary types."
  ;; Empty/null alist
  (fiveam:is (equalp (vector #xa0)
                     (neo4cl::encode-alist '())))
  ;; With contents
  (let ((lst '(("one" . "eins"))))
    (fiveam:is (equalp (vector #xA1
                               #x83 #x6F #x6E #x65
                               #x84 #x65 #x69 #x6E #x73)
                       (neo4cl::encode-alist lst)))))

(fiveam:test
  encode-element
  "Serialise whatever."
  ;; Boolean
  (fiveam:is (equalp (vector neo4cl::+packstream-true+)
                     (neo4cl::encode-element :true)))
  (fiveam:is (equalp (vector neo4cl::+packstream-false+)
                     (neo4cl::encode-element :false)))
  ;; Null
  (fiveam:is (equalp (vector #xc0) (neo4cl::encode-element :null)))
  ;; Integers
  (fiveam:is (equalp (vector #x2a) (neo4cl::encode-element 42)))
  (fiveam:is (equalp (vector #xf2) (neo4cl::encode-element -14)))
  (fiveam:is (equalp (vector #xc9 #x02 #x9A) (neo4cl::encode-element 666)))
  ;; Strings
  (fiveam:is (equalp (vector #x80) (neo4cl::encode-element "")))
  (fiveam:is (equalp (vector #x84 #x41 #x42 #x43 #x44)
                     (neo4cl::encode-element "ABCD")))
  ;; List
  (fiveam:is (equalp (vector #x92 #x84 #x41 #x42 #x43 #x44 #x2a)
                     (neo4cl::encode-element (list "ABCD" 42))))
  ;; Hash-table
  (let ((hash (make-hash-table :test #'equal)))
    (setf (gethash "one" hash) "eins")
    (fiveam:is (equalp (vector #xA1
                               #x83 #x6F #x6E #x65
                               #x84 #x65 #x69 #x6E #x73)
                       (neo4cl::encode-element hash))))
  ;; alist
  (let ((lst '(("one" . "eins"))))
    (fiveam:is (equalp (vector #xA1
                               #x83 #x6F #x6E #x65
                               #x84 #x65 #x69 #x6E #x73)
                       (neo4cl::encode-alist lst)))))


;;; Parsing/decoding

(fiveam:test
  identify-marker
  "Can you tell what it is yet?"
  (fiveam:is (equal "Null" (neo4cl::identify-marker #xc0)))
  (fiveam:is (equal "Boolean" (neo4cl::identify-marker #xc2)))
  (fiveam:is (equal "Boolean" (neo4cl::identify-marker #xc3)))
  (fiveam:is (equal "Integer" (neo4cl::identify-marker #xf3)))
  (fiveam:is (equal "Integer" (neo4cl::identify-marker #x7e)))
  (fiveam:is (equal "Float" (neo4cl::identify-marker #xc1)))
  (fiveam:is (equal "Bytes" (neo4cl::identify-marker #xcd)))
  (fiveam:is (equal "String" (neo4cl::identify-marker #x85)))
  (fiveam:is (equal "String" (neo4cl::identify-marker #xd1)))
  (fiveam:is (equal "List" (neo4cl::identify-marker #x97)))
  (fiveam:is (equal "List" (neo4cl::identify-marker #xd6)))
  (fiveam:is (equal "Dictionary" (neo4cl::identify-marker #xa6)))
  (fiveam:is (equal "Dictionary" (neo4cl::identify-marker #xda)))
  (fiveam:is (equal "Structure" (neo4cl::identify-marker #xbb))))

(fiveam:test
  parse-ints
  "Parsing integers from a string of octets"
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-int (vector #x03) 0)
    (fiveam:is (equal 3 result))
    (fiveam:is (equal 0 len))
    (fiveam:is (equal 1 hdrlen)))
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-int (vector #x7b) 0)
    (fiveam:is (equal 123 result))
    (fiveam:is (equal 0 len))
    (fiveam:is (equal 1 hdrlen)))
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-int (vector #xc8 #x7b) 0)
    (fiveam:is (equal 123 result))
    (fiveam:is (equal 1 len))
    (fiveam:is (equal 1 hdrlen)))
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-int (vector #xc8 #xd6) 0)
    (fiveam:is (equal -42 result))
    (fiveam:is (equal 1 len))
    (fiveam:is (equal 1 hdrlen)))
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-int (vector #xc9 #x7b #x2c) 0)
    (fiveam:is (equal 31532 result))
    (fiveam:is (equal 2 len))
    (fiveam:is (equal 1 hdrlen)))
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-int (vector #xca #x7b #x2c #x69 #x13) 0)
    (fiveam:is (equal 2066508051 result))
    (fiveam:is (equal 4 len))
    (fiveam:is (equal 1 hdrlen)))
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-int (vector #xcb
                                #x7b #x2c #x69 #x13
                                #x7b #x2c #x69 #x13) 0)
    (fiveam:is (equal 8875584498032208147 result))
    (fiveam:is (equal 8 len))
    (fiveam:is (equal 1 hdrlen)))
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-int (vector #xcb
                                #x7f #xff #xff #xff
                                #xff #xff #xff #xff) 0)
    (fiveam:is (equal 9223372036854775807 result))
    (fiveam:is (equal 8 len))
    (fiveam:is (equal 1 hdrlen)))
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-int (vector #xcb
                                #x80 #x00 #x00 #x00
                                #x00 #x00 #x00 #x00) 0)
    (fiveam:is (equal -9223372036854775808 result))
    (fiveam:is (equal 8 len))
    (fiveam:is (equal 1 hdrlen))))

(fiveam:test
  string-length
  "Deriving the lengths of strings, and calculating new vector-offsets."
  ;; Short string at start of vector.
  (fiveam:is (equal '(3 1) (neo4cl::get-string-length (vector #x83 #x41 #x42 #x43) 0)))
  ;; Short string at offset within vector.
  (fiveam:is (equal '(3 1) (neo4cl::get-string-length (vector #x00 #x00 #x83 #x41 #x42 #x43) 2)))
  ;; 8-bit string-length at start of vector
  (fiveam:is (equal '(25 2)
                    (neo4cl::get-string-length
                      (vector #xD0 #x19
                              #x41 #x42 #x43 #x44 #x45 #x46 #x47 #x49 #x50
                              #x51 #x52 #x53 #x54 #x55 #x56 #x57 #x59 #x60
                              #x61 #x62 #x63 #x64 #x65)
                      0)))
  (fiveam:is (equal '(26 2)
                    (neo4cl::get-string-length
                      (vector #x00 #x00 #x00 #xD0 #x1A
                              #x41 #x42 #x43 #x44 #x45 #x46 #x47 #x49 #x50
                              #x51 #x52 #x53 #x54 #x55 #x56 #x57 #x59 #x60
                              #x61 #x62 #x63 #x64 #x65 #x66)
                      3)))
  ;; 16-bit string length at start of vector
  ;; No, the string itself isn't that long, but we're not actually parsing it anyway.
  (fiveam:is (equal '(4680 3)
                    (neo4cl::get-string-length
                      (vector #xD1 #x12 #x48
                              #x41 #x42 #x43 #x44 #x45 #x46 #x47 #x49 #x50
                              #x51 #x52 #x53 #x54 #x55 #x56 #x57 #x59 #x60
                              #x61 #x62 #x63 #x64 #x65)
                      0)))
  ;; 32-bit string length at start of vector
  ;; No, the string itself isn't that long, but we're not actually parsing it anyway.
  (fiveam:is (equal '(167987218 5)
                    (neo4cl::get-string-length
                      (vector #xD2 #x12 #x48 #x03 #x0A
                              #x41 #x42 #x43 #x44 #x45 #x46 #x47 #x49 #x50
                              #x51 #x52 #x53 #x54 #x55 #x56 #x57 #x59 #x60
                              #x61 #x62 #x63 #x64 #x65)
                      0))))

(fiveam:test
  parse-string
  "Parsing a string from a vector of octets."
  :depends-on 'string-length
  ;; Simplest case: short string at start of vector
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-string (vector #x83 #x66 #x6f #x6f) 0)
    (fiveam:is (equal "foo" result))
    (fiveam:is (equal 3 len))
    (fiveam:is (equal 1 hdrlen)))
  ;; Short string at offset within vector
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-string (vector #x00 #x00 #x83 #x66 #x6f #x6f) 2)
    (fiveam:is (equal "foo" result))
    (fiveam:is (equal 3 len))
    (fiveam:is (equal 1 hdrlen))))

(fiveam:test
  parse-list
  "Parse a heterogeneous list from a vector of octets."
  :depends-on '(decode-string)
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-list (vector #x92
                                 #x83 #x4f #x6e #x65
                                 #x83 #x54 #x77 #x6F)
                         0)
    (fiveam:is (equal (list "One" "Two") result))
    (fiveam:is (equal 8 len))
    (fiveam:is (equal 1 hdrlen))))

(fiveam:test
  parse-dictionary
  "Parsing a dictionary from a vector of octets."
  :depends-on 'parse-string
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-dictionary
      (vector
        #xa2  ; Dictionary with max=size=2
        ;; Entry 1: server version
        #x86 #x73 #x65 #x72 #x76 #x65 #x72
        #x8b #x4e #x65 #x6f #x34 #x6a #x2f #x34 #x2e #x33 #x2e #x39
        ;; Entry 2: session ID
        #x8d #x63 #x6f #x6e #x6e #x65 #x63 #x74 #x69 #x6f #x6e #x5f #x69 #x64
        #x88 #x62 #x6f #x6c #x74 #x2d #x31 #x33 #x30)
      0)
    (fiveam:is (equal "bolt-130" (gethash "connection_id" result)))
    (fiveam:is (equal "Neo4j/4.3.9" (gethash "server" result)))
    (fiveam:is (equal 42 len))
    (fiveam:is (equal 1 hdrlen))))

(fiveam:test
  parse-element
  "Test the general-purpose parsing dispatcher."
  :depends-on '(parse-string parse-dictionary)
  ;; Null
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-element (vector #xc0) 0)
    (fiveam:is (equal nil result))
    (fiveam:is (equal 0 len))
    (fiveam:is (equal 1 hdrlen)))
  ;; Boolean
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-element (vector #xc3) 0)
    (fiveam:is (not (null result)))
    (fiveam:is (zerop len))
    (fiveam:is (equal 1 hdrlen)))
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-element (vector #xc2) 0)
    (fiveam:is (null result))
    (fiveam:is (zerop len))
    (fiveam:is (equal 1 hdrlen)))
  ;; Integer
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-element (vector #x03) 0)
    (fiveam:is (equal 3 result))
    (fiveam:is (zerop len))
    (fiveam:is (equal 1 hdrlen)))
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-element (vector #x7b) 0)
    (fiveam:is (equal 123 result))
    (fiveam:is (zerop len))
    (fiveam:is (equal 1 hdrlen)))
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-element (vector #xc8 #x7b) 0)
    (fiveam:is (equal 123 result))
    (fiveam:is (equal 1 len))
    (fiveam:is (equal 1 hdrlen)))
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-element (vector #xc9 #x7b #x2c) 0)
    (fiveam:is (equal 31532 result))
    (fiveam:is (equal 2 len))
    (fiveam:is (equal 1 hdrlen)))
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-element (vector #xca #x7b #x2c #x69 #x13) 0)
    (fiveam:is (equal 2066508051 result))
    (fiveam:is (equal 4 len))
    (fiveam:is (equal 1 hdrlen)))
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-element (vector #xcb
                                               #x7b #x2c #x69 #x13
                                               #x7b #x2c #x69 #x13) 0)
    (fiveam:is (equal 8875584498032208147 result))
    (fiveam:is (equal 8 len))
    (fiveam:is (equal 1 hdrlen)))
  ;; Float
  ;; Bytes
  ;; String
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-element (vector #x83 #x66 #x6f #x6f) 0)
    (fiveam:is (equal "foo" result))
    (fiveam:is (equal 3 len))
    (fiveam:is (equal 1 hdrlen)))
  ;; List
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-element (vector #x92
                                    #x83 #x4f #x6e #x65
                                    #x83 #x54 #x77 #x6F) 0)
    (fiveam:is (equal (list "One" "Two") result))
    (fiveam:is (equal 8 len))
    (fiveam:is (equal 1 hdrlen)))
  ;; Dictionary
  (multiple-value-bind (result len hdrlen)
    (neo4cl::decode-element (vector
               #xa2  ; Dictionary with max=size=2
               ;; Entry 1: server version
               #x86 #x73 #x65 #x72 #x76 #x65 #x72
               #x8b #x4e #x65 #x6f #x34 #x6a #x2f #x34 #x2e #x33 #x2e #x39
               ;; Entry 2: session ID
               #x8d #x63 #x6f #x6e #x6e #x65 #x63 #x74 #x69 #x6f #x6e #x5f #x69 #x64
               #x88 #x62 #x6f #x6c #x74 #x2d #x31 #x33 #x30) 0)
    (fiveam:is (equal "bolt-130" (gethash "connection_id" result)))
    (fiveam:is (equal "Neo4j/4.3.9" (gethash "server" result)))
    (fiveam:is (equal 42 len))
    (fiveam:is (equal 1 hdrlen)))
  ;; Structure
  )
