(asdf:defsystem #:neo4cl-test
  :serial t
  :license "MIT license"
  :author "James Fleming <james@electronic-quill.net>"
  :description "Test suite for neo4cl"
  :depends-on (#:neo4cl
               #:fiveam)
  :components ((:file "package")
               (:file "neo4cl-test")))
