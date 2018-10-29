(defpackage :clnote/app
  (:use :common-lisp
        :clnote/db
        :unix-opts)
  (:shadow unix-opts:describe)
  (:export :main))

(in-package :clnote/app)


(defconstant default-notes-path
  (merge-pathnames #P".clnote/notes"
                   (user-homedir-pathname)))


;; output utilities
(defun print-topics ()
  "Print available tags."
  (dolist (topic (db:get-tags))
    (format t "  * ~(~a~) (~D)~%"
            (getf topic :tag)
            (getf topic :count))))


(defun print-notes (tag)
  "View all notes for specific tag."
  (let ((notes (reverse (db:get-notes tag)))
        (count -1))
    (format t "  * on book ~(~a~)~%" tag)
    (dolist (note notes)
      (format t "  (~D) ~a~%"
              (incf count)
              (getf note :text)))))


;; common utilities
(defun topicp (x)
  (and x (symbolp x)))


;; commands 
(defstruct command
  (id nil :read-only t :type symbol)
  (names nil :read-only t :type cons)
  (action nil :read-only t :type function))


(defun run-view (&rest args)
  (load-notes default-notes-path)
  (let ((tag-str (first args)))
    (declare (type (or string null) tag-str))
    (if tag-str
        (print-notes (read-from-string tag-str))
        (print-topics))))


(defun run-add (&rest args)
  (let ((topic-arg (first args))
        (content (second (member "-c" args :test #'equal))))
    (unless (and
             topic-arg
             content
             (not (equal topic-arg "-c")))
      (format t "  * Incorrect number of arguments~%")
      (return-from run-add))
    (unless (stringp content)
      (format t "  * Incorrect format of content~%"))
    (let ((topic (read-from-string topic-arg)))
      (unless (topicp topic)
        (format t "  * Incorrect format of topic argument~%")
        (return-from run-add))
      (load-notes default-notes-path)
      (add-note (make-note topic content))
      (store-notes default-notes-path)
      (format t "  v added to ~A~%~%" topic-arg)
      (format t "--------------------content--------------------~%")
      (format t "~A~%" content)
      (format t "-----------------------------------------------~%"))))


(defparameter commands
  (list
   (make-command
    :id 'view
    :names '("view" "v")
    :action #'run-view)
   (make-command
    :id 'add
    :names '("a" "new" "n")
    :action #'run-add)))


(defun parse-cmd (name)
  "Returns command executor by its string NAME."
  (find-if #'(lambda (x)
               (member name (command-names x) :test #'equal))
           commands))


(defun run-cmd (cmd args)
  "Run command CMD passing it given ARGS."
  (apply (command-action cmd) args))


(defun main- (program argv)
  (let* ((cmd-name (first argv))
         (cmd (parse-cmd cmd-name)))
    (cond
      ((null cmd-name)
       (format t "Usage:~%  ~A [command]~%" program))
      (cmd
       (run-cmd cmd (rest argv)))
      (t
       (format t "  * unknown command ~S for ~S~%"
               cmd-name program)))))

(defun main ()
  (let ((program (first (opts:argv)))
        (argv (rest (opts:argv))))
;;    (dolist (arg argv)
;;      (format t "'~S' ('~A')~%" arg arg))
    (main- program argv)))
