;CL-BINARY-FILE is published under the MIT license
;Copyright (c) 2008 Sami Makinen
;
;Permission is hereby granted, free of charge, to any person obtaining a copy of 
;this software and associated documentation files (the "Software"), to deal in 
;the Software without restriction, including without limitation the rights to 
;use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
;of the Software, and to permit persons to whom the Software is furnished to do 
;so, subject to the following conditions:
;
;The above copyright notice and this permission notice shall be included in all 
;copies or substantial portions of the Software.
;
;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
;OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
;SOFTWARE.


(in-package #:neo4cl)

;;; The following code has been swiped wholesale from cl-binary-file, then slightly modified.
;;; - hence the MIT license block above.
;;;
;;; I added type declarations after the fact.

(defun twos-complement-signed (value bytes &aux (bits (* bytes 8)) (sign-bit (1- bits)))
  "Returns signed integer. Value is in two's complement format and
   result has bytes * 8 bits."
  (declare (type integer value bytes bits sign-bit))
  (if (logbitp sign-bit value) ; If sign bit is set, convert it to negative.
      ;; (1+ (lognot value)) won't work because lisp sees result as integer of size bits+1.
      ;; So the conversion is done such that Lisp understands the type and sign of the value.
      (1- (- (logandc2 (1- (ash 1 bits)) value))) 
      value))

(defun twos-complement (value bytes)
  "Returns two's complement of given value. Bytes is size of value in bytes."
  (declare (type integer value bytes))
  (if (<= value 0)
      (ldb (byte (* 8 bytes) 0) value)
      (twos-complement-signed value bytes)))

(defun int-bytes (int bytes &aux (bytes-1 (1- bytes)))
  "Returns bytes from given (positive) integer least significant byte first."
  (declare (type integer int bytes))
  (if (<= bytes-1 0)
    (cons int nil)
    (cons (logand #xff int) (int-bytes (ash int -8) bytes-1))))
