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


(defparameter current-version 1)


(defvar *notes* nil)


(defparameter default-notes-path
  (merge-pathnames #P".clnote/notes"
                   (user-homedir-pathname)))


;; constructors and accessors
(defun make-note (topic
                  note
                  &optional (timestamp (local-time:now)))
  "Constructs new note object for given parameters.
By default TIMESTAMP is now."
  (list :topic topic
        :timestamp timestamp
        :text note))


(defun add-note (note)
  (when (not *notes*)
    (init-notes-db)
    (setf *notes*
          (list :version current-version :notes nil)))
  (push note (getf *notes* :notes))
  note)


(defun get-all-notes () 
  (getf *notes* :notes))


(defun get-notes (topic)
  "Get all notes for specific topic."
  (remove-if-not (lambda (note) (equal topic (getf note :topic))) (get-all-notes)))


(defun get-topics ()
  "Get unique topic names and count of notes for each topic."
  (let* ((topics (mapcar (lambda (x) (getf x :topic)) (get-all-notes)))
         (unique-topics (remove-duplicates topics)))
    (mapcar (lambda (x) (list :topic x :count (count x topics))) unique-topics)))


(defun remove-notes (topic)
  "Remove notes for specific TOPIC."
  (labels ((to-remove-p (x)
             (eq topic (getf x :topic))))
    (let ((count (count-if #'to-remove-p (get-all-notes))))
      (setf (getf *notes* :notes) (remove-if #'to-remove-p (get-all-notes)))
      count)))


(defun init-notes-db ()
  (setf *notes*
        (list :version current-version :notes nil)))


;; output 
(defun dump-notes ()
  "Dump notes in debug format."
  (dolist (note (get-all-notes))
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
        (let ((new-notes-db (read in)))
          ;; upgrade old DB formats 
          (cond
            ((not (member :version new-notes-db))
             (init-notes-db)
             (setf (getf *notes* :notes) new-notes-db))
            (t (setf *notes* new-notes-db))))
        (init-notes-db)))
  *notes*)
