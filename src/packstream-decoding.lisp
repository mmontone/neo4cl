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


;;;; Packstream deserialisation library
;;;;
;;;; "PackStream is a general purpose data serialisation format,
;;;;  originally inspired by (but incompatible with) MessagePack."
;;;; For more details, see https://7687.org/packstream/packstream-specification-1.html

(in-package #:neo4cl)


;;; Note that `decode-<type>` functions all expect a vector of octets and return 3 values:
;;; - the deserialised object, in CL-native form
;;; - the number of octets occupied by the object itself (excluding its marker byte(s))
;;; - the number of octets occupied by the object's marker bytes.
;;; This is to make it nice and easy to calculate the starting position within the vector of the
;;; next element to be deserialised, via `multiple-value-bind`.

(defun identify-marker (marker)
  "What kind of thing does this marker denote? Returns a string with its name.
   Doesn't provide any details about how to handle it, just what *kind* of thing it is,
   so we can decide what to do next."
  (declare (type (unsigned-byte 8) marker))
  (cond
    ((equal #xc0 marker) "Null")
    ((or (member marker (list #xc2 #xc3))) "Boolean")
    ((or (member (ldb (byte 4 4) marker) (list #xf #x0 #x1 #x2 #x3 #x4 #x5 #x6 #x7))
         (member marker (list #xc8 #xc9 #xca #xcb)))
     "Integer")
    ((equal #xc1 marker) "Float")
    ((member marker (list #xcc #xcd #xce)) "Bytes")
    ((or (equal #x8 (ldb (byte 4 4) marker))
         (member marker (list #xd0 #xd1 #xd2))) "String")
    ((or (equal #x9 (ldb (byte 4 4) marker))
         (member marker (list #xd4 #xd5 #xd6))) "List")
    ((or (equal #xa (ldb (byte 4 4) marker))
         (member marker (list #xd8 #xd9 #xda))) "Dictionary")
    ((equal #xb (ldb (byte 4 4) marker)) "Structure")
    ;; If we got here, I have NFI WTF it is.
    (t nil)))

(defun identify-structure-node (tag)
  "What kind of structure is this? Return its name as a string.
   Note that each tag-byte corresponds to an ASCII character hinting at the name."
  (declare (type (unsigned-byte 8) tag))
  ;; If we got here, I have NFI WTF it is.
  (cond
    ((equal #x4e tag) "Node")  ; #\N
    ((equal #x52 tag) "Relationship")  ; #\R
    ((equal #x72 tag) "UnboundRelationship")  ; #\r
    ((equal #x50 tag) "Path")  ; #\P
    ((equal #x44 tag) "Date")  ; #\D
    ((equal #x54 tag) "Time")  ; #\T
    ((equal #x74 tag) "LocalTime")  ; #\t
    ((equal #x66 tag) "DateTimeZoneId")  ; #\f
    ((equal #x45 tag) "Duration")  ; #\E
    ((equal #x58 tag) "Point2D")  ; #\X
    ((equal #x59 tag) "Point3D")  ; #\Y
    (t nil)))

(defun get-string-length (vec offset)
  "Convenience function, wrapped around get-element-length."
  (declare (type vector vec)
           (type integer offset))
  (get-element-length vec offset #x8 #xd0 #xd1 #xd2))

(defun get-list-length (vec offset)
  "Convenience function, wrapped around get-element-length."
  (declare (type vector vec)
           (type integer offset))
  (get-element-length vec offset #x9 #xd4 #xd5 #xd6))

(defun get-element-length (vec offset mtiny m8 m16 m32)
  "Parse the 1/2/3-octet marker in a vector, using the offset to locate the start of the marker.
   Return a 2-element list:
   - integer: length of the element.
   - integer: marker-length.
   By calculating and returning the marker's length here, we avoid the need for the client function
   to derive it after the fact, from the element itself.
   Arguments:
   - Vector of unsigned-byte octets.
   - Starting position of the marker within the vector.
   - Upper nibble for identifying 8-bit length markers.
   - Octet for identifying 16-bit markers, in #xnn hexadecimal format.
   - Octet for identifying 32 markers, in #xnn hexadecimal format."
  (declare (type vector vec)
           (type integer offset))
  ;(log-message :debug (format nil "Extracting the length of an element at offset ~D." offset))
  (let ((marker (aref vec offset)))
    (cond
      ;; Tiny element. Length is in lower 4 bits.
      ((equal mtiny (ldb (byte 4 4) marker))
       ;(log-message :debug "Tiny element: length is in the lower nibble of the marker.")
       (let ((len (dpb (ldb (byte 4 0) marker)
                                 (byte 4 0)
                                 0)))
         ;(log-message :debug (format nil "Length: ~D octets." len))
         (list len 1)))
      ;; Longer element: 8-bit integer
      ((equal m8 marker)
       ;(log-message :debug (format nil "Length in 16-255 octet range."))
       ;; Get the length in a single hit, because it's simple when it's 1 octet
       (let ((len (dpb (aref vec (+ offset 1))
                                 (byte 8 0)
                                 0)))
         ;(log-message :debug (format nil "Length: ~D octets." len))
         (list len 2)))
      ;; Longer string: 16-bit integer
      ((equal m16 marker)
       ;(log-message :debug (format nil "Length in 256-65535 octet range."))
       ;; Take a couple more steps to get the length, because now we need to poll 2 octets
       (let ((len 0))
         (setf len (dpb (aref vec (+ offset 1)) (byte 8 8) len))
         (setf len (dpb (aref vec (+ offset 2)) (byte 8 0) len))
         ;(log-message :debug (format nil "Length: ~D octets." len))
         (list len 3)))
      ;; Longer string: 32 integer
      ((equal m32 marker)
       ;(log-message :debug (format nil "Length > 65535 octets."))
       ;; Take a couple more steps to get the length, because now we need to poll 2 octets
       (let ((len 0))
         (setf len (dpb (aref vec (+ offset 1)) (byte 8 0) len))
         (setf len (dpb (aref vec (+ offset 2)) (byte 8 8) len))
         (setf len (dpb (aref vec (+ offset 3)) (byte 8 16) len))
         (setf len (dpb (aref vec (+ offset 4)) (byte 8 24) len))
         ;(log-message :debug (format nil "Length: ~D octets." len))
         (list len 5)))
      ;; Fall back to error
      (t (error 'packstream-error :category "marker"
                :message (format nil "Invalid length marker '#x~2,'0X" marker))))))

(defun decode-int (vec offset)
  "Parse an integer from the vector, using the starting offset to find it.
  Return a 3-element list:
  - integer: the number itself.
  - integer: the number of octets in the vector occupied by the integer.
  - integer: the number of octets occupied by the header."
  (declare (type vector vec)
           (type integer offset))
  ;(log-message :debug (format nil "Parsing an integer at offset ~D" offset))
  (let ((marker (aref vec offset)))
    (cond (
           ;; Positive tiny int
           (<= 0 marker 127)
           (values marker 0 1))
          ;; Negative tiny int
          ((equal #xf (ldb (byte 4 4) marker))
           (values (- (ldb (byte 4 0) marker) 16) 0 1))
          ;; Positive 8-bit integer
          ((equal #xc8  marker)
           (values (aref vec (+ offset 1)) 1 1))
          ;; Positive 16-bit integer
          ((equal #xc9 marker)
           (let ((result 0))
             (setf (ldb (byte 8 8) result) (aref vec (+ offset 1)))
             (setf (ldb (byte 8 0) result) (aref vec (+ offset 2)))
             (values result 2 1)))
          ;; Positive 32-bit integer
          ((equal #xca marker)
           (let ((result 0))
             (setf (ldb (byte 8 24) result) (aref vec (+ offset 1)))
             (setf (ldb (byte 8 16) result) (aref vec (+ offset 2)))
             (setf (ldb (byte 8 8) result) (aref vec (+ offset 3)))
             (setf (ldb (byte 8 0) result) (aref vec (+ offset 4)))
             (values result 4 1)))
          ;; Positive 64-bit integer
          ((equal #xcb marker)
           (let ((result 0))
             (setf (ldb (byte 8 56) result) (aref vec (+ offset 1)))
             (setf (ldb (byte 8 48) result) (aref vec (+ offset 2)))
             (setf (ldb (byte 8 40) result) (aref vec (+ offset 3)))
             (setf (ldb (byte 8 32) result) (aref vec (+ offset 4)))
             (setf (ldb (byte 8 24) result) (aref vec (+ offset 5)))
             (setf (ldb (byte 8 16) result) (aref vec (+ offset 6)))
             (setf (ldb (byte 8 8) result) (aref vec (+ offset 7)))
             (setf (ldb (byte 8 0) result) (aref vec (+ offset 8)))
             (values result 8 1)))
          ;; Fall back to error
          (t
            (error 'bolt-error
                   :category "parse" :message
                   "Unhandled integer.")))))

(defun decode-string (vec offset)
  "Parse a string from the vector, using the starting offset to find it.
   Return 3 values:
   - string: the string itself
   - integer: the number of octets in the vector occupied by the string.
   - integer: the number of octets occupied by the header.
   It's true that you could get the number of octets via `length`, but I'm including this here for consistency with the rest of the decode-<type> functions. This avoids the need to keep track of different return values for different types."
  (declare (type vector vec)
           (type integer offset))
  ;(log-message :debug (format nil "Parsing a string at offset ~D" offset))
  (let ((stringlength (get-string-length vec offset)))
    ;; Construct the return value
    (values
      ;; The string itself
      (trivial-utf-8:utf-8-bytes-to-string
        ;; The vector to read it from.
        vec
        ;; Starting point: initial offset, plus the length of the string's marker.
        :start (+ offset (second stringlength))
        ;; Number of octets to parse into the string: string-length as reported in the marker.
        :end (+ offset (first stringlength) (second stringlength)))
      ;; Bytes occupied by the string
      (first stringlength)
      ;; Bytes occupied by the marker
      (second stringlength))))

(defun decode-list (vec offset)
  "Parse a list from the vector, using the starting offset to find it.
   Return 3 values:
   - list the list itself.
   - integer: the number of octets in the vector occupied by the list.
   - integer: the number of octets occupied by the header."
  (declare (type vector vec)
           (type integer offset))
  ;(log-message :debug (format nil "Parsing a list at offset ~D" offset))
  (let* ((length-details (get-list-length vec offset))
         (entry-count (first length-details))
         (header-length (second length-details))
         ;; Pointer keeps track of the next start-offset within the vector.
         ;; Start it at the beginning of the first entry;
         ;; we'll update it at the end of each iteration of the loop.
         (pointer (+ offset header-length))
         ;; Return-value accumulator
         (acc '()))
    (loop for i from 1 to entry-count
          do (multiple-value-bind (result len hdrlen)
               (decode-element vec pointer)
               ;; So now we've done the `let` section...
               ;; Update the pointer
               (push result acc)
               ;; Update the `dict` alist with what we found
               (setf pointer (+ pointer len hdrlen))))
    ;; Return the result
    (values (nreverse acc)
          ;; Calculate the bytes by adding the header-length to the initial offset,
          ;; then subtracting that sum from the final value of the pointer.
          (- pointer (+ offset header-length))
          header-length)))

(defun decode-dictionary (vec offset)
  "Parse a Dictionary structure from a byte-vector, given its starting offset.
   Return 3 values:
   - hash-table: the Dictionary contents. Keys are always strings, so the test is #'equal.
   - integer: the number of bytes the dict occupies in the vector, _excluding_ its header.
   - integer: number of bytes the header occupies in the vector.
   It's an alist because the Packstream spec states that a Dictionary can contain multiple instances of the same key; an alist can accommodate this, but a hash-table can't."
  (declare (type vector vec)
           (type integer offset))
  ;(log-message :debug (format nil "Parsing a Dictionary at offset ~D" offset))
  (let* ((dict-size-elements (get-element-length vec offset #xa #xd8 #xd9 #xda))
         (entry-count (first dict-size-elements))
         (header-length (second dict-size-elements))
         ;; Pointer keeps track of the next start-offset within the vector.
         ;; Start it at the beginning of the first entry;
         ;; we'll update it at the end of each iteration of the loop.
         (pointer (+ offset header-length))
         ;; Return-value accumulator
         (hash (make-hash-table :test #'equal
                                :size (first dict-size-elements))))
    ;(log-message :debug (format nil "Dictionary has ~D entries." entry-count))
    ;; Using iteration mostly because recursion is unlikely to mix well with very large Dictionaries.
    ;; Iteration is probably also faster, but we can tune for performance later.
    (loop for i from 1 to entry-count
          do (multiple-value-bind (key keylen key-hdrlen)
               (decode-string vec pointer)
               (multiple-value-bind (value valuelen value-hdrlen)
                 (decode-element vec (+ pointer
                                        ;; Add the length of the first string
                                        keylen
                                        ;; Add the marker-length of the first string
                                        key-hdrlen))
                 ;; So now we've done the `let` section...
                 ;(log-message :debug "Updating the Dictionary pointer")
                 (setf pointer (+ pointer
                                  ;; Add string and marker lengths for the key
                                  keylen key-hdrlen
                                  ;; Add string and marker lengths for the value
                                  valuelen value-hdrlen))
                 ;(log-message :debug (format nil "Dictionary pointer value is now ~D." pointer))
                 ;; Update the `hash` table with what we found
                 ;(log-message :debug (format nil "Updating the return alist with key ~A." (first key-details)))
                 (setf (gethash key hash) value))))
    ;; Return the `hash` alist
    (values hash
            ;; Calculate the bytes by subtracting the header-length and current pointer
            ;; from the initial offset.
            (- pointer offset header-length)
            header-length)))

(defun vector-to-node (arr)
  "Take a 3-element array, and return a 'node instance."
  (make-instance 'node
                 :node-id (aref arr 0)
                 :node-labels (aref arr 1)
                 :node-properties (aref arr 2)))

(defun vector-to-relationship (arr)
  "Take a 5-field array, and return a 'relationship instance."
  (make-instance 'relationship
                 :relationship-id (aref arr 0)
                 :start-node-id (aref arr 1)
                 :end-node-id (aref arr 2)
                 :relationship-type (aref arr 3)
                 :relationship-properties (aref arr 4)))

(defun decode-structure (vec offset)
  "Unpack a structure. Returns a 3-element list:
   - A 1-dimensional array, with an element in that dimension for each field in the structure.
   - integer: the number of octets in the vector occupied by the element itself.
   - integer: the number of octets occupied by the element's marker.
   Note that this function assumes <16 fields, because v1 of the Packstream spec doesn't define a
   marker byte for larger structures."
  (declare (type vector vec)
           (type integer offset))
  (let* (;; Extract the number of fields in the structure,
         ;; from the lower 4 bits of the marker byte.
         (num-fields (ldb (byte 4 0) (aref vec offset)))
         ;; Create an intermediate accumulator
         (acc (make-array num-fields))
         ;; Extract the tag-byte for later reference
         (tag-byte (aref vec (+ offset 1)))
         ;; Initialise the pointer used for iterating through the vector,
         ;; and then for calculating the structure's size in octets.
         (pointer (+ offset 2)))
    (log-message :debug (format nil "Decoding a #x~X Structure with ~D elements."
                                tag-byte num-fields))
    ;; Extract the fields, and insert them into the accumulator
    (loop for i from 0 to (- num-fields 1)
          do (multiple-value-bind (value len hdrlen)
               (decode-element vec pointer)
               (setf (aref acc i) value)
               (setf pointer (+ pointer len hdrlen))))
    ;; Return the result, counting the tag-byte as part of the header, not part of the payload.
    (values
      ;; Create a valid return value from the vector,
      ;; dispatching on the tag-byte.
      (cond
        ;; Tag-byte = N for node
        ((equal #x4e tag-byte)
         (vector-to-node acc))
        ;; Tag-byte = R for relationship
        ((equal #x52 tag-byte)
         (vector-to-relationship acc))
        ;; Fall back to error
        (t
         (error 'packstream-error
                :category "parsing"
                :message (format nil "Unhandled tag-byte #x~X" tag-byte))))
      ;; Calculate the number of octets occupied by the structure within the vector.
      (- pointer offset 2)
      ;; Header-length = marker byte + tag-byte = 2 octets
      2)))


(defun decode-element (vec offset)
  "General dispatcher for parsing a Packstream element, given a vector of octets and the starting offset.
   Returns a 3-element list:
   - the parsed object, as whatever CL type is most applicable.
   - integer: the number of octets in the vector occupied by the element itself.
   - integer: the number of octets occupied by the element's marker."
  (declare (type vector vec)
           (type integer offset))
  (let ((element-type (identify-marker (aref vec offset))))
    ;(log-message :debug (format nil "Parsing an element of type ~A, from offset ~D." element-type offset))
    (cond
      ;; Null
      ((equal "Null" element-type)
       ;(log-message :debug "Element-type was Null.")
       (values nil 0 1))
      ;; Boolean
      ((equal "Boolean" element-type)
       ;(log-message :debug "Element-type was Boolean")
       (values (equal #xc3 (aref vec offset)) 0 1))
      ;; Integer
      ((equal "Integer" element-type) (decode-int vec offset))
      ;; Float
      ;; Bytes
      ;; String
      ((equal "String" element-type) (decode-string vec offset))
      ;; List
      ((equal "List" element-type) (decode-list vec offset))
      ;; Dictionary
      ((equal "Dictionary" element-type) (decode-dictionary vec offset))
      ;; Structure
      ((equal #xb (ldb (byte 4 4) (aref vec offset)))
       (decode-structure vec offset))
      ;; Fall back to error
      (t (error 'packstream-error
                :category "parser"
                :message (format nil "Unhandled element type ~A" element-type))))))
