;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; Logging infrastructure
;;; We only need something extremely simple

(in-package #:neo4cl)

(defvar *loglevels*
  '(:crit 4
    :critical 4
    :error 3
    :warn 2
    :warning 2
    :info 1
    :debug 0))

;; Set the threshold logging level
(defparameter *loglevel* :debug)

(defparameter *log-stream*
  (make-synonym-stream 'cl:*standard-output*))

(defun make-timestamp ()
  (multiple-value-bind (sec minute hour date month year)
    (get-decoded-time)
    (format nil "[~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d]"
    year month date hour minute sec)))

(defun log-message (severity message)
  (declare (type keyword severity))
  (when (>= (getf *loglevels* severity)
            (getf *loglevels* *loglevel*))
    (format *log-stream* "~%~A ~A ~A"
            (make-timestamp)
            severity
            message)))
