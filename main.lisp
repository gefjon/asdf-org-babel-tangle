(uiop:define-package :asdf-org-babel-tangle/main
    (:use :cl)
  (:nicknames :asdf-org-babel-tangle)
  (:import-from :asdf
   :cl-source-file :sideway-operation :upward-operation :input-files
   :output-files :component-pathname :component-name :perform
   :component-depends-on :compile-op :load-op :make-operation)
  (:import-from :uiop :run-program)
  (:export :org-source-file :tangle-op))
(cl:in-package :asdf-org-babel-tangle/main)

(defclass org-source-file (cl-source-file)
  ((type :initform "org"))
  (:documentation "component class for an org-mode literate text file that can tangled into a common lisp source file"))

(defclass tangle-op (sideway-operation upward-operation)
  ()
  (:documentation "use org-babel-tangle to convert an org-source-file into a cl-source-file"))

(defmethod input-files ((op tangle-op) (file org-source-file))
  "a `TANGLE-OP' on an `ORG-SOURCE-FILE' takes the file as input"
  (list (component-pathname file)))

(defmethod output-files ((op tangle-op) (file org-source-file))
  "a `TANGLE-OP' on an `ORG-SOURCE-FILE' produces a lisp file named like FILE"
  (list (merge-pathnames (make-pathname :type "lisp")
                         (component-pathname file))))

(defmethod input-files ((op compile-op) (file org-source-file))
  "a `COMPILE-OP' on an `ORG-SOURCE-FILE' takes as input the output of the associated `TANGLE-OP'"
  (output-files (make-operation 'tangle-op) file))

(defun tangle (source-path output-path)
  "invoke an external `emacs' to tangle SOURCE-PATH into OUTPUT-PATH"
  (let ((command (format nil
                         "emacs -batch ~s --eval '(org-babel-tangle nil ~s ~s)'"
                         (namestring source-path)
                         (namestring output-path)
                         "lisp")))
    (run-program command)))

(defmethod perform ((op tangle-op) (file org-source-file))
  (let* ((input-file (first (input-files op file)))
         (output-file (first (output-files op file))))
    (tangle input-file output-file)))

(defmethod component-depends-on ((op compile-op) (file org-source-file))
  `((tangle-op ,(component-name file))
    ,@(call-next-method)))

(defmethod component-depends-on ((op load-op) (file org-source-file))
  `((tangle-op ,(component-name file))
    ,@(call-next-method)))
