(defpackage :clnote/db
  (:nicknames :db)
  (:use :common-lisp
        :local-time)
  (:export :default-notes-path
           :make-note
           :add-note
           :get-tags
           :get-notes
           :store-notes
           :load-notes))

(in-package :clnote/db)

(local-time:enable-read-macros)


(defvar *notes* nil)

(defparameter default-notes-path
  (merge-pathnames #P".clnote/notes"
                   (user-homedir-pathname)))


;; constructors
(defun make-note (tag
                  note
                  &optional (timestamp (local-time:now)))
  "Constructs new note object for given parameters.
By default TIMESTAMP is now."
  (list :tag tag
        :timestamp timestamp
        :text note))

;; access
(defun add-note (note)
  (push note *notes*)
  note)

(defun get-notes (topic)
  "Get all notes for specific tag."
  (remove-if-not (lambda (note) (equal topic (getf note :tag))) *notes*))


(defun get-tags ()
  "Get unique tag names and count of notes for each tag."
  (let* ((tags (mapcar (lambda (x) (getf x :tag)) *notes*))
         (unique-tags (remove-duplicates tags)))
    (mapcar (lambda (x) (list :tag x :count (count x tags))) unique-tags)))



(defun clear-notes ()
  (setf *notes* nil))


;; output 
(defun dump-notes ()
  "Dump notes in debug format."
  (dolist (note *notes*)
    (format t "~{~a:~12t~a~%~}~%" note)))


;; save and load
(defun store-notes (&optional (path default-notes-path))
  "Store notes to given path."
  (ensure-directories-exist path)
  (with-open-file (out path
                       :direction :output
                       :if-exists :supersede)
    (pprint *notes* out))
  t)

(defun load-notes (&optional (path default-notes-path))
  "Loads notes from given path."
  (with-open-file (in path
                      :direction :input
                      :if-does-not-exist nil)
    (if in
        (setf *notes* (read in))
        (setf *notes* nil)))
  *notes*)
