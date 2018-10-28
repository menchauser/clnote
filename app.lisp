(defpackage :clnote/app
  (:use :common-lisp
        :clnote/db)
  (:export :main))

(in-package :clnote/app)


(defun main ()
  (format t "Hi mom!~%"))
