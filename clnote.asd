(asdf:defsystem "clnote"
  :description "clnote: a simple notes storage"
  :version "0.1.0"
  :author "Mukhamed Karanashev <menchauser@gmail.com>"
  :license "MIT License"
  :depends-on ("local-time"
               "unix-opts")
  :serial t
  :components ((:file "db")
               (:file "app"))
  ;; for building
  :build-operation "program-op"
  :build-pathname "clnote"
  :entry-point "clnote/app:main")
  
