(defpackage :clnote/app
  (:use :common-lisp
        :clnote/db
        :unix-opts)
  (:shadow unix-opts:describe)
  (:export :main))

(in-package :clnote/app)


(defparameter default-clnote-name "clnote")


;;;; common utilities
(defun safe-read-from-string (x)
  (when x
      (read-from-string x)))


;; output utilities
(defun print-topics ()
  "Print available topics."
  (dolist (topic (db:get-topics))
    (format t "  * ~(~a~) (~D)~%"
            (getf topic :topic)
            (getf topic :count))))


(defun print-note (topic number)
  (let* ((notes (reverse (db:get-notes topic)))
         (note (nth number notes)))
    (if note
        (progn
          (format t "  * topic name: ~(~a~)~%" topic)
          (format t "  * created at: ~A~%~%" (getf note :timestamp))
          (print-content (getf note :text)))
        (format t "  * no note ~S in topic ~(~A~)~%" number topic))))


(defun print-notes (topic &optional number)
  "View all notes for specific topic."
  (cond (number (print-note topic number))
        (t 
         (let ((notes (reverse (db:get-notes topic)))
               (count -1))
           (format t "  * on topic ~(~a~)~%" topic)
           (dolist (note notes)
             (format t "  (~D) ~a~%"
                     (incf count)
                     (getf note :text)))))))


(defun print-content (content) 
  (format t "------------------------content------------------------~%")
  (format t "~A~%" content)
  (format t "-------------------------------------------------------~%"))


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
  (format t "  ~A view lisp~%" default-clnote-name)
  (format t "  * List all notes~%")
  (format t "  ~A view --all~%" default-clnote-name))


(defun run-view (&rest args)
  (opts:define-opts
    (:name :help
           :description "show help"
           :short #\h
           :long "help")
    (:name :all
           :description "view all notes"
           :showrt #\a
           :long "all"))
  (multiple-value-bind (options free-args)
      (opts:get-opts args)
    (cond
      ((getf options :help)
       (print-view-usage))
      ((getf options :all)
       (db:load-notes)
       (let ((started nil))
         (dolist (topic-info (db:get-topics))
           (if (not started)
               (setf started t)
               (format t "~%"))
           (print-notes (getf topic-info :topics) nil))))
      (t 
       (let ((topic (safe-read-from-string (first free-args)))
             (index (safe-read-from-string (second free-args))))
         (db:load-notes)
         ;; add args check because when args are NIL
         ;; unix-opts is going to implicitly use unix-opts:argv
         (if (and args topic)
             (print-notes topic index)
             (print-topics)))))))


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
    (cond
      ((getf options :help)
       (print-add-usage))
      (t  
       (let ((topic-arg (first free-args))
             (content (getf options :content)))
         (unless (and topic-arg content)
           (format t "  * Incorrect number of arguments~%")
           (return-from run-add))
         (let ((topic (read-from-string topic-arg)))
           (unless (symbolp topic)
             (format t "  * Incorrect format of topic argument~%")
             (return-from run-add))
           (db:load-notes)
           (db:add-note (db:make-note topic content))
           (db:store-notes)
           (format t "  v added to ~A~%~%" topic-arg)
           (print-content content)))))))


;; remove
(defun print-remove-usage ()
  (format t "Remove a note or topic~%~%")
  (format t "Usage:~%")
  (format t "  ~A remove <topic name> <note index>~%" default-clnote-name)
  (format t "  ~A remove -t <topic name>~%~%" default-clnote-name)
  (format t "Aliases:~%")
  (format t "  remove, rm, d, delete~%~%")
  (format t "Examples:~%~%")
  (format t "  * Delete a note by its index from a topic~%")
  (format t "  ~A delete lisp 2~%~%" default-clnote-name)
  (format t "  * Delete a whole topic~%")
  (format t "  ~A delete -t lisp~%~%" default-clnote-name)
  (format t "Flags:~%~%")
  (format t "  -t, --topic string~22TTopic name to delete~%")
  (format t "  -h, --help        ~22THelp for remove~%"))


(defun run-remove (&rest args)
  (opts:define-opts
    (:name :help
           :description "show help"
           :short #\h
           :long "help")
    (:name :topic
           :description "topic name to delete"
           :short #\t
           :long "topic"
           :arg-parser #'read-from-string
           :meta-var "TOPIC"))
  (multiple-value-bind (options free-args)
      (opts:get-opts args)
    (cond
      ((getf options :help)
       (print-remove-usage))
      ((getf options :topic)
       (let ((topic (getf options :topic)))
         (when (y-or-n-p "Are you sure want to remove whole topic ~(~A~)?" topic)
           (db:load-notes)
           (let ((removed-count (db:remove-notes topic)))
             (db:store-notes)
             (format t "Topic '~(~A~)' with ~A notes removed" topic removed-count)))))
      (t
       (let ((topic (safe-read-from-string (first free-args)))
             (index (safe-read-from-string (second free-args))))
         (if (and args
                  (symbolp topic)
                  (numberp index))
             (progn
               (when (y-or-n-p "Are you sure want to remove note ~A from topic ~(~A~)?" index topic)
                 (db:load-notes)
                 (db:store-notes)))
             (format t "  * Incorrect number of arguments~%")))))))


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
