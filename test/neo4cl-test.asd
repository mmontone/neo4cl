(asdf:defsystem #:neo4cl-test
  :serial t
  :description "Test suite for neo4cl"
  :author "James Fleming <james@electronic-quill.net>"
  :license "As yet unlicensed"
  :depends-on (#:neo4cl
               #:fiveam)
  :components ((:file "package")
               (:file "neo4cl-test")))

