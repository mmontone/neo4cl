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


;;;; Packstream serialisation library
;;;;
;;;; "PackStream is a general purpose data serialisation format, originally inspired by (but incompatible with) MessagePack."
;;;; For more details, see https://7687.org/packstream/packstream-specification-1.html


;;; Notes about the serialisation strategy
;;;
;;; Atoms are serialised into vectors.
;;; Compound structures are serialised into _lists_ of vectors, to be written in turn
;;; to the output stream.


(in-package #:neo4cl)


;;;; Functions and methods

;;; Encode objects for transmission
;;;
;;; `encode-<type>` functions all return a vector of hexadecimal-encoded values,
;;; ready for writing to an output stream.

(defun encode-null ()
  "Return a vector containing a Packstream Null."
  (vector +packstream-null+))

(defun encode-boolean (bool)
  "Return a vector containing a Packstream Boolean.
   :true evaluates to true, and :false evaluates to false. Otherwise, the argument will be evaluated as a normal boolean."
  (vector (cond
            ((equal :true bool)
             +packstream-true+)
            ((equal :false bool)
             +packstream-false+)
            ((null bool)
             +packstream-false+)
            (t
             +packstream-true+))))

;; FIXME: encode negative non-tiny integers
(defun encode-integer (int)
  "Encode an integer into a vector containing a packstream Int."
  (declare (type integer int))
  (cond
    ;; Positive Tiny Int
    ((<= 0 int 127)
     (vector int))
    ;; Positive 16-bit integer: INT_16
    ;; Going straight to this because TINYINT already covers all the positive 8-bit values.
    ((<= 128 int 32767)
     (vector #xc9 (ldb (byte 8 8) int) (ldb (byte 8 0) int)))
    ;; Positive 32 integer: INT_32
    ((<= 32768 int 2147483647)
     (vector #xca
             (ldb (byte 8 24) int)
             (ldb (byte 8 16) int)
             (ldb (byte 8 8) int)
             (ldb (byte 8 0) int)))
    ;; Positive 32 integer: INT_32
    ((<= 2147483648 int 9223372036854775807)
     (vector #xcb
             (ldb (byte 8 56) int)
             (ldb (byte 8 48) int)
             (ldb (byte 8 40) int)
             (ldb (byte 8 32) int)
             (ldb (byte 8 24) int)
             (ldb (byte 8 16) int)
             (ldb (byte 8 8) int)
             (ldb (byte 8 0) int)))
    ;; Negative Tiny Int
    ((<= -16 int -1)
     (vector (dpb (ldb (byte 4 0) (+ 16 int)) (byte 4 0) #xF0)))
    ;; Fall back to failure
    (t (error "Integer out of range."))))

(defun encode-string (str)
  "Packstream type: String. Description: unicode text, UTF-8"
  (declare (type string str))
  ;(log-message :debug "Packstream-encoding a string")
  (let ((strlen (trivial-utf-8:utf-8-byte-length str)))
    (cond
      ;; Empty string
      ((string= "" str)
       (vector #x80))
      ;; Small string: 1-15 bytes long
      ((< strlen 16)
       (let ((marker 128))
         (setf (ldb (byte 4 0) marker) strlen)
         (concatenate 'vector (vector marker)
                      (trivial-utf-8:string-to-utf-8-bytes str))))
      ;; String 16-255 bytes
      ((<= 16 strlen 255)
       (concatenate 'vector
                    (vector #xd0   ; Denotes 8-bit string-length
                            (ldb (byte 8 0) strlen))
                    (trivial-utf-8:string-to-utf-8-bytes str)))
      ((<= 256 strlen 65535)
       (concatenate 'vector
                    (vector #xd1   ; Denotes 16-bit string-length
                            (ldb (byte 8 8) strlen)
                            (ldb (byte 8 0) strlen))
                    (trivial-utf-8:string-to-utf-8-bytes str)))
      ((> 65535 strlen)
       (concatenate 'vector
                    (vector #xd2   ; Denotes 32-bit string-length
                            (ldb (byte 8 24) strlen)
                            (ldb (byte 8 16) strlen)
                            (ldb (byte 8 8) strlen)
                            (ldb (byte 8 0) strlen))
                    (trivial-utf-8:string-to-utf-8-bytes str)))
      ;; Fall back to failure
      (t
        (error (format nil "Unhandled string '~A'" str))))))

(defun vectorise-list (lst)
  "Take a list of values, and concatenate them all into a single vector.
   Beware: uses recursion, so excessively long lists run the risk of blowing the stack."
  ;(log-message :debug "Vectorising a list.")
  (if (null (cdr lst))
      (car lst)
      (concatenate 'vector (car lst) (vectorise-list (cdr lst)))))

(defun encode-list (lst)
  "Packstream type: List."
  (declare (type list lst))
  ;(log-message :debug "Packstream-encoding a list.")
  ;; Special-case for the empty list.
  (if (null lst)
      (vector #x90)
      ;; Non-empty list? Carry on.
      (let* ((len (list-length lst))
             (marker
               (cond
                 ((< len 16)
                  (vector (dpb (ldb (byte 4 0) len) (byte 4 0) #x90)))
                 ((<= 16 len 255)
                  (vector #xd4 (ldb (byte 8 0) len)))
                 ((<= 256 len 65535)
                  (vector #xd5 (ldb (byte 8 8) len) (ldb (byte 8 0) len)))
                 ((<= 65536 len 2147483647)
                  (vector #xd6
                          (ldb (byte 8 24) len)
                          (ldb (byte 8 16) len)
                          (ldb (byte 8 8) len)
                          (ldb (byte 8 0) len)))
                 (t
                  (error 'packstream-error
                         :category "encoding"
                         :message "List too long.")))))
        (concatenate 'vector marker (vectorise-list
                                      (mapcar #'encode-element lst))))))

;; A Dictionary is a list containing key-value entries.
;; - Keys must be a String.
;; - Can contain multiple instances of the same key.
;; - Permit a mixture of types.
;; The size of a Dictionary denotes the number of key-value entries within that Dictionary,
;; not the total packed byte size.
(defun encode-hash-table (hash)
  "Packstream type: Dictionary."
  (declare (type hash-table hash))
  (let ((hash-size (hash-table-count hash)))  ; Pre-calculate the hash-table size for convenience
    ;; Special-case for an empty/null hash-table
    (if (zerop hash-size)
        (vector #xa0)
        ;; Accumulator for the result.
        ;; Initialise it on creation with the Dictionary size
        (let ((acc (list (cond
                           ((< hash-size 16)
                            (vector (dpb (ldb (byte 4 0) hash-size)
                                         (byte 4 0)
                                         #xa0)))
                           ((< hash-size 256)
                            (vector #xd8 hash-size))
                           ((< hash-size 65536)
                            (vector #xd9 hash-size))
                           (t
                            (vector #xda hash-size))))))
          ;; Serialise the entries.
          ;; Use the classic push/nreverse pattern.
          (maphash #'(lambda (key value)
                       (push (encode-string key) acc)
                       (push (encode-element value) acc))
                   hash)
          ;; Return the result
          (vectorise-list (nreverse acc))))))

(defun alist-p (lst)
  "Check whether the argument is actually an alist."
  (and (listp lst)
       (or (null lst)
           (every #'(lambda (pair)
                      (and (consp pair)
                           (atom (car pair))
                           (atom (cdr pair))))
                  lst))))

(defun encode-alist (lst)
  "Packstream type: Dictionary.
   Assumes a proper alist, in which the car of each pair is a string."
  (declare (type list lst))
  (let ((len (list-length lst)))
    ;; Special-case for an empty/null hash-table
    (if (zerop len)
        (vector #xa0)
        ;; Accumulator for the result.
        ;; Initialise it on creation with the Dictionary size
        (let ((acc (list (cond
                           ((< len 16)
                            (vector (dpb (ldb (byte 4 0) len)
                                         (byte 4 0)
                                         #xa0)))
                           ((< len 256)
                            (vector #xd8 len))
                           ((< len 65536)
                            (vector #xd9 len))
                           (t
                            (vector #xda len))))))
          ;; Serialise the entries.
          ;; Use the classic push/nreverse pattern.
          (mapcar #'(lambda (pair)
                      (push (encode-string (car pair)) acc)
                      (push (encode-element (cdr pair)) acc))
                  lst)
          ;; Return the result
          (vectorise-list (nreverse acc))))))

(defun encode-element (element)
  "General-purpose dispatching function for encoding things into Packstream format.
   Note that an empty list will be encoded as Null, not as a zero-element list."
  (cond
    ((equal :null element)
     (encode-null))
    ((member element '(:true :false))
     (encode-boolean element))
    ((typep element 'integer)
     (encode-integer element))
    ((typep element 'string)
     (encode-string element))
    ((alist-p element)
     (encode-alist element))
    ((typep element 'list)
     (encode-list element))
    ((typep element 'hash-table)
     (encode-hash-table element))
    (t
     (error 'packstream-error
            :category "encode"
            :message (format nil "Unhandled element-type: ~A." (type-of element))))))
