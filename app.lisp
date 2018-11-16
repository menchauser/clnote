(defpackage :clnote/app
  (:use :common-lisp
        :clnote/db
        :unix-opts
        :uiop)
  (:shadow unix-opts:describe)
  (:export :main))

(in-package :clnote/app)


(defparameter default-clnote-name "clnote")


;;;; common utilities
(defun read-from-string-or-nil (x)
  (when x
      (read-from-string x)))


(defun has-help-option (argv)
  (intersection argv '("-h" "--help")
                :test #'equal))


(defun trim-space (string)
  "Trims space characters from beginning and end of STRING."
  (string-trim '(#\Space #\Tab #\Newline) string))


(defun string-blank-p (string)
  "Checks if STRING is blank (NIL, empty or consists only of space characters"
  (or (not string)
      (= 0 (length (trim-space string)))))


;;;; work with external editor
(defun get-editor-cmd (filename)
  (let* ((editor (uiop:getenv "EDITOR"))
         (resolved-editor-cmd
          (cond ((equal editor "vim") '("vim"))
                (t '("vi")))))
    ;; formatting to string is required to convert pathname to
    ;; acceptable format as CLI argument
    (append resolved-editor-cmd (list (format nil "~A" filename)))))


(defun run-editor ()
  (uiop:with-temporary-file (:pathname path :keep nil :prefix "clnote")
    (let ((cmd (get-editor-cmd path)))
      (multiple-value-bind (output error-output result)
          (uiop:run-program cmd
                            :input :interactive
                            :output :interactive
                            :error-output :interactive)
        (declare (ignore output)
                 (ignore error-output))
        (when (zerop result)
          ;; strange behaviour: there's always one more newline symbol
          ;; at the end of the read text so we remove it
          (string-right-trim '(#\Newline) 
                             (read-file-string path)))))))


;;;; output utilities
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
(defparameter view-usage
  '("List topics, notes or view a content"
    ""
    "Usage:"
    "  clnote view <topic name?> <note index?>"
    ""
    "Aliases:"
    "  view, v"
    ""
    "Examples:"
    ""
    "  * View all topics"
    "  clnote view"
    "  * List notes for a topic"
    "  clnote view lisp"
    "  * List all notes"
    "  clnote view --all"))


(defun run-view (&rest args)
  (opts:define-opts
    (:name :all
           :description "view all notes"
           :showrt #\a
           :long "all"))
  (multiple-value-bind (options free-args)
      (opts:get-opts args)
    (cond
      ((getf options :all)
       (db:load-notes)
       (let ((started nil))
         (dolist (topic-info (db:get-topics))
           (if (not started)
               (setf started t)
               (format t "~%"))
           (print-notes (getf topic-info :topics) nil))))
      (t 
       (let ((topic (read-from-string-or-nil (first free-args)))
             (index (read-from-string-or-nil (second free-args))))
         (db:load-notes)
         ;; add args check because when args are NIL
         ;; unix-opts is going to implicitly use unix-opts:argv
         (if (and args topic)
             (print-notes topic index)
             (print-topics)))))))


;; add 
(defparameter add-usage
  '("Add a note to topic"
    ""
    "Usage:"
    "  clnote add <topic name> -c <content>"
    ""
    "Aliases:"
    "  add, a, new, n"
    ""
    "Examples:"
    "  * Add a simple note"
    "  clnote add lisp -c \"to parse multiple return values from a function use 'multiple-values-bind'\""))


(defun run-add (&rest args)
  (labels
      ((perform-add (topic content)
         (db:load-notes)
         (db:add-note (db:make-note topic content))
         (db:store-notes)
         (format t "  v added to ~(~A~)~%~%" topic)
         (print-content content)))
    (opts:define-opts
      (:name :content
             :description "note content"
             :short #\c
             :long "content"
             :arg-parser #'identity
             :meta-var "CONTENT"))
    (multiple-value-bind (options free-args)
        (opts:get-opts args)
      (let ((topic-arg (first free-args))
            (content (getf options :content)))
        (unless topic-arg
          (format t "  * Incorrect number of arguments (topic must be present)~%")
          (return-from run-add))
        (let ((topic (read-from-string topic-arg)))
          (unless (symbolp topic)
            (format t "  * Incorrect format of topic argument~%")
            (return-from run-add))
          ;; if content is given via -c key - use it
          ;; otherwise: run editor and get result from one
          ;; do not create note when given empty string
          (if content
              (if (string-blank-p content)
                  (format t "  * Incorrect: note text is empty~%")
                  (perform-add topic content))
              (let ((external-content (run-editor)))
                (if (string-blank-p external-content)
                    (format t "  * Incorrect: note text is empty~%")
                    (perform-add topic external-content)))))))))


;; remove
(defparameter remove-usage
  '("Remove a note or topic"
    ""
    "Usage:"
    "  clnote remove <topic name> <note index>"
    "  clnote remove -t <topic name>"
    ""
    "Aliases:"
    "  remove, rm, d, delete"
    ""
    "Examples:"
    ""
    "  * Delete a note by its index from a topic"
    "  clnote delete lisp 2"
    ""
    "  * Delete a whole topic"
    "  clnote delete -t lisp"
    ""
    "Flags:"
    ""
    "  -t, --topic string  Topic name to delete"
    "  -h, --help          Help for remove"))


(defun run-remove (&rest args)
  (opts:define-opts
    (:name :topic
           :description "topic name to delete"
           :short #\t
           :long "topic"
           :arg-parser #'read-from-string
           :meta-var "TOPIC"))
  (multiple-value-bind (options free-args)
      (opts:get-opts args)
    (cond
      ((getf options :topic)
       (let ((topic (getf options :topic)))
         (when (y-or-n-p "Are you sure want to remove whole topic ~(~A~)?" topic)
           (db:load-notes)
           (let ((removed-count (db:remove-notes topic)))
             (db:store-notes)
             (format t "Topic '~(~A~)' with ~A notes removed" topic removed-count)))))
      (t
       (let ((topic (read-from-string-or-nil (first free-args)))
             (index (read-from-string-or-nil (second free-args))))
         (if (and args
                  (symbolp topic)
                  (numberp index))
             (progn
               (db:load-notes)
               (let* ((topic-notes (reverse (db:get-notes topic)))
                      (note-to-remove (nth index topic-notes)))
                 (if note-to-remove
                     (progn
                       (print-content (getf note-to-remove :text))
                       (when (y-or-n-p "Are you sure want to remove note ~A from topic ~(~A~)?" index topic)
                         (db:remove-note (getf note-to-remove :id))
                         (db:store-notes)))
                     (format t "Topic ~(~A~) has no note ~A~%" topic index))))
             (format t "  * Incorrect number of arguments~%")))))))


(defstruct command
  (id nil :read-only t :type symbol)
  (description nil :read-only t :type string)
  (names nil :read-only t :type cons)
  (action nil :read-only t :type function)
  (usage nil :read-only t :type cons))


(defparameter *commands*
  (list
   (make-command
    :id 'add
    :description "Add new note to a topic"
    :names '("add" "a" "new" "n")
    :action #'run-add
    :usage add-usage)
   (make-command
    :id 'view
    :description "List topics or notes"
    :names '("view" "v")
    :action #'run-view
    :usage view-usage)
   (make-command
    :id 'remove
    :description "Remove a note or topic within note"
    :names '("remove" "rm" "delete" "d")
    :action #'run-remove
    :usage remove-usage)))


(defun parse-cmd (name)
  "Returns command executor by its string NAME."
  (cond
    ((null name) nil)
    (t (find-if #'(lambda (x)
                    (member name (command-names x) :test #'equal))
                *commands*))))


(defun run-cmd (cmd args)
  "Run command CMD passing it given ARGS."
  (apply (command-action cmd) args))


(defun print-usage (program)
  (format t "Usage:~%")
  (format t "  ~A [command] [options]~%~%" program)
  (format t "Available commands:~%")
  (dolist (cmd *commands*)
    (format t "  ~A~16T~A~%"
            (string-downcase (symbol-name (command-id cmd)))
            (command-description cmd))))


(defun print-cmd-usage (cmd)
  (dolist (usage-line (command-usage cmd))
    (format t "~A~%" usage-line)))

  
(defun main- (program argv)
  (let* ((cmd-name (first argv))
         (cmd (parse-cmd cmd-name)))
    (cond
      ((null cmd-name)
       (print-usage program))
      (cmd ;; when command argument is defined
       (if (has-help-option argv)
           ;; print usage when -h argument is given
           (print-cmd-usage cmd)
           (run-cmd cmd (rest argv))))
      ((has-help-option argv)
       (print-usage program))
      (t
       (format t "  * unknown command ~S for ~S~%"
               cmd-name program)
       (print-usage program)))))


(defun main ()
  (let* ((program (car (opts:argv)))
         (argv (rest (opts:argv))))
    (main- program argv)))
