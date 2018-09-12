(defpackage :clnote
  (:use :common-lisp
        :local-time
        :uiop)
  (:export :main))

(in-package :clnote)

(local-time:enable-read-macros)


(defvar *notes* nil)


;; constructors
(defun make-note (tag note timestamp)
  (list :tag tag
        :timestamp timestamp
        :text note))

(defun build-note (tag note)
  "Construct new note object for current time."
  (make-note tag note (local-time:now)))


;; access
(defun add-note (note)
  (push note *notes*))

(defun get-notes (tag)
  "Get all notes for specific tag."
  (remove-if-not (lambda (note) (equal tag (getf note :tag))) *notes*))


;; output 
(defun dump-notes (notes)
  (dolist (note notes)
    (format t "~{~a:~12t~a~%~}~%" note)))


;; save and load
(defun store-notes (path)
  "Store notes to given path."
  (with-open-file (out path
                       :direction :output
                       :if-exists :supersede)
    (pprint *notes* out)))

(defun load-notes (path)
  "Loads notes from given path."
  (with-open-file (in path
                      :direction :input
                      :if-does-not-exist :error)
    (setf *notes* (read in))))


;; main
(defun main ()
  (format t "Hello~%")
  (format t "~a~%" uiop:*command-line-arguments*))
