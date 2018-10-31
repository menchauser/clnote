(defpackage :clnote/app
  (:use :common-lisp
        :clnote/db
        :unix-opts)
  (:shadow unix-opts:describe)
  (:export :main))

(in-package :clnote/app)



(defparameter default-clnote-name "clnote")


;; output utilities
(defun print-topics ()
  "Print available topics."
  (dolist (topic (db:get-tags))
    (format t "  * ~(~a~) (~D)~%"
            (getf topic :tag)
            (getf topic :count))))


(defun print-notes (topic)
  "View all notes for specific topic."
  (let ((notes (reverse (db:get-notes topic)))
        (count -1))
    (format t "  * on topic ~(~a~)~%" topic)
    (dolist (note notes)
      (format t "  (~D) ~a~%"
              (incf count)
              (getf note :text)))))


;;;; commands

;; view
(defun print-view-usage ()
  (format t "List topics, notes or view a content~%~%")
  (format t "Usage:~%")
  (format t "  ~A view <topic name?> <note index?>~%~%" default-clnote-name)
  (format t "Aliases:~%")
  (format t "  view, v~%~%")
  (format t "Examples:~%~%")
  (format t "  * View all topics~%")
  (format t "  ~A view~%" default-clnote-name)
  (format t "  * List notes for a topic~%")
  (format t "  ~A view lisp~%" default-clnote-name))


(defun run-view (&rest args)
  (opts:define-opts
    (:name :help
           :description "show help"
           :short #\h
           :long "help"))
  (multiple-value-bind (options free-args)
      (opts:get-opts args)
    (when (getf options :help)
      (print-view-usage))
    (let ((topic-str (first free-args)))
      (declare (type (or string null) topic-str))
      (db:load-notes)
      ;; add args check because when args are NIL
      ;; unix-opts is going to implicitly use unix-opts:argv
      (if (and args topic-str)
          (print-notes (read-from-string topic-str))
          (print-topics)))))


;; add 
(defun print-add-usage ()
  (format t "Add a note to topic~%~%")
  (format t "Usage:~%")
  (format t "  ~A add <topic name> -c <content>~%~%" default-clnote-name)
  (format t "Aliases:~%")
  (format t "  add, a, new, n~%~%")
  (format t "Examples:~%~%")
  (format t "  * Add a simple note~%")
  (format t "  ~A add lisp -c \"to parse multiple return values from a function use 'multiple-values-bind'\"~%" default-clnote-name)) 


(defun run-add (&rest args)
  (opts:define-opts
    (:name :help
           :description "show help"
           :short #\h
           :long "help")
    (:name :content
           :description "note content"
           :short #\c
           :long "content"
           :arg-parser #'identity
           :meta-var "CONTENT"))
  (multiple-value-bind (options free-args)
      (opts:get-opts args)
    (when (getf options :help)
      (print-add-usage))
    (let ((topic-arg (first free-args))
          (content (getf options :content)))
      (unless (and topic-arg content)
        (format t "  * Incorrect number of arguments~%")
        (return-from run-add))
      (let ((topic (read-from-string topic-arg)))
        (unless (symbolp topic)
          (format t "  * Incorrect format of topic argument~%")
          (return-from run-add))
        (load-notes)
        (add-note (make-note topic content))
        (store-notes)
        (format t "  v added to ~A~%~%" topic-arg)
        (format t "--------------------content--------------------~%")
        (format t "~A~%" content)
        (format t "-----------------------------------------------~%")))))
  

(defstruct command
  (id nil :read-only t :type symbol)
  (description nil :read-only t :type string)
  (names nil :read-only t :type cons)
  (action nil :read-only t :type function))


(defparameter commands
  (list
   (make-command
    :id 'add
    :description "Add new note to a topic"
    :names '("add" "a" "new" "n")
    :action #'run-add)
   (make-command
    :id 'view
    :description "List topics or notes"
    :names '("view" "v")
    :action #'run-view)))


(defun parse-cmd (name)
  "Returns command executor by its string NAME."
  (cond
    ((null name) nil)
    (t (find-if #'(lambda (x)
                    (member name (command-names x) :test #'equal))
                commands))))


(defun run-cmd (cmd args)
  "Run command CMD passing it given ARGS."
  (apply (command-action cmd) args))


(defun print-usage (program)
  (format t "Usage:~%")
  (format t "  ~A [command] [options]~%~%" program)
  (format t "Available commands:~%")
  (dolist (cmd commands)
    (format t "  ~A~16T~A~%"
            (string-downcase (symbol-name (command-id cmd)))
            (command-description cmd))))
  
(defun main- (program argv)
  (let* ((cmd-name (first argv))
         (cmd (parse-cmd cmd-name)))
    (cond
      ((null cmd-name) (print-usage program))
      (cmd
       (run-cmd cmd (rest argv)))
      ((member "-h" argv :test #'equal) (print-usage program))
      (t
       (format t "  * unknown command ~S for ~S~%"
               cmd-name program)
       (print-usage program)))))


(defun main ()
  (let* ((program (car (opts:argv)))
         (argv (rest (opts:argv))))
    (main- program argv)))
