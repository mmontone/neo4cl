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

(defun integer-to-bytes (value &optional (bytes 1) (marker #xc8))
  "Transform an integer into a list of 8-bit unsigned bytes.
   Intended for use with negative integers.
   The `bytes` argument is the number of octets in the output vector,
   so for a 16-bit number you should specify 2.
   The `marker` argument should be one of #xc8, #xc9, #xca or #xcb.
   Heavily inspired by cl-binary-file:write-integer."
  (declare (type integer value bytes))
  ;; Sanity checks
  (when (< bytes 1)
    (error "bytes argument must be >0."))
  (when (> (/ (integer-length value) 8.0) bytes)
    (error "Value ~D is too big to fit in ~D bytes" value bytes))
  ;; Generate the result
  (make-array
    (1+ bytes)
    :element-type '(unsigned-byte 8)
    ;; int-bytes returns its output in LSB format, while Packstream is a big-endian format.
    :initial-contents (cons
                        marker
                        (reverse (int-bytes (twos-complement value bytes) bytes)))))

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
    ;; Negative 8-bit integer: INT_8
    ;; TINYINT only covers the first nibble's worth.
    ((<= -128 int -17)
     (integer-to-bytes int 1 #xc8))
    ;; Negative 16-bit integer
    ((<= -32768 int -129)
     (integer-to-bytes int 2 #xc9))
    ;; Negative 32-bit integer
    ((<= -2147483648 int -32769)
     (integer-to-bytes int 4 #xca))
    ;; Negative 64-bit integer
    ((<= -9223372036854775808 int -2147483649)
     (integer-to-bytes int 8 #xcb))
    ;; Fall back to failure
    (t (error "Integer out of range."))))

(defun encode-float (num)
  "Encode floating-point numbers into 64-bit binary format, per IEEE 754."
  (declare (type float num))
  (let ((val (ieee-floats:encode-float64 num)))
    (vector #xc1
            (ldb (byte 8 56) val)
            (ldb (byte 8 48) val)
            (ldb (byte 8 40) val)
            (ldb (byte 8 32) val)
            (ldb (byte 8 24) val)
            (ldb (byte 8 16) val)
            (ldb (byte 8 8) val)
            (ldb (byte 8 0) val))))

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


(defgeneric encode-element (element)
  (:documentation "Encode a thing into Packstream format. Note that an empty list will be encoded as Null, not as a zero-element list."))

(defmethod encode-element ((element null))
  (encode-null))

(defmethod encode-element ((element (eql :null)))
  (encode-null))

(defmethod encode-element ((element (eql :true)))
  (encode-boolean :true))

(defmethod encode-element ((element (eql :false)))
  (encode-boolean :false))

(defmethod encode-element ((element integer))
  (encode-integer element))

(defmethod encode-element ((element double-float))
  (encode-float element))

(defmethod encode-element ((element float))
  (encode-float (coerce element 'double-float)))

(defmethod encode-element ((element string))
  (encode-string element))

(defmethod encode-element ((element hash-table))
  (encode-hash-table element))

(defmethod encode-element ((element list))
  (if (alist-p element)
      (encode-alist element)
      (encode-list element)))

(defmethod encode-element ((element date))
  (vectorise-list
    (list
      (vector #xb1) ; 1-field structure
      (vector #x44) ; Date
      (encode-integer (days element)))))

(defmethod encode-element ((element timestructure))
  (vectorise-list
    (list
      (vector #xb2)
      (vector #x54)
      (encode-integer (nanoseconds element))
      (encode-integer (tz-offset-seconds element)))))

(defmethod encode-element ((element localtime))
  (vectorise-list
    (list
      (vector #xb1)
      (vector #x74)
      (encode-integer (nanoseconds element)))))

(defmethod encode-element ((element datetime))
  (vectorise-list
    (list
      (vector #xb3)
      (vector #x46)
      (encode-integer (seconds element))
      (encode-integer (nanoseconds element))
      (encode-integer (tz-offset-seconds element)))))

(defmethod encode-element ((element datetimezoneid))
  (vectorise-list
    (list
      (vector #xb3)
      (vector #x66)
      (encode-integer (seconds element))
      (encode-integer (nanoseconds element))
      (encode-string (tz-id element)))))

(defmethod encode-element ((element localdatetime))
  (vectorise-list
    (list
      (vector #xb2)
      (vector #xd)
      (encode-integer (seconds element))
      (encode-integer (nanoseconds element)))))

(defmethod encode-element ((element duration))
  (vectorise-list
    (list
      (vector #xb4)
      (vector #x45)
      (encode-integer (months element))
      (encode-integer (days element))
      (encode-integer (seconds element))
      (encode-integer (nanoseconds element)))))

(defmethod encode-element ((element point2d))
  (vectorise-list
    (list
      (vector #xb3)
      (vector #x58)
      (encode-integer (srid element))
      (encode-float (x element))
      (encode-float (y element)))))

(defmethod encode-element ((element point3d))
  (vectorise-list
    (list
      (vector #xb4)
      (vector #x59)
      (encode-integer (srid element))
      (encode-float (x element))
      (encode-float (y element))
      (encode-float (z element)))))
