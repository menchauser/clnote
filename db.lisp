(defpackage :clnote/db
  (:nicknames :db)
  (:use :common-lisp
        :local-time)
  (:export :default-notes-path
           :make-note
           :add-note
           :get-topics
           :get-notes
           :remove-notes
           :store-notes
           :load-notes))

(in-package :clnote/db)

(local-time:enable-read-macros)


(defvar *notes* nil)

(defparameter default-notes-path
  (merge-pathnames #P".clnote/notes"
                   (user-homedir-pathname)))


;; constructors
(defun make-note (topic
                  note
                  &optional (timestamp (local-time:now)))
  "Constructs new note object for given parameters.
By default TIMESTAMP is now."
  (list :topic topic
        :timestamp timestamp
        :text note))

;; access
(defun add-note (note)
  (push note *notes*)
  note)


(defun get-notes (topic)
  "Get all notes for specific topic."
  (remove-if-not (lambda (note) (equal topic (getf note :topic))) *notes*))


(defun get-topics ()
  "Get unique topic names and count of notes for each topic."
  (let* ((topics (mapcar (lambda (x) (getf x :topic)) *notes*))
         (unique-topics (remove-duplicates topics)))
    (mapcar (lambda (x) (list :topic x :count (count x topics))) unique-topics)))


(defun remove-notes (topic)
  "Remove notes for specific TOPIC."
  (labels ((to-remove-p (x)
             (eq topic (getf x :topic))))
    (let ((count (count-if #'to-remove-p *notes*)))
      (setf *notes* (remove-if #'to-remove-p *notes*))
      count)))


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
