(uiop:define-package :asdf-org-babel-tangle/main
    (:use :cl)
  (:nicknames :asdf-org-babel-tangle)
  (:import-from :asdf
   :cl-source-file :sideway-operation :upward-operation :input-files :output-files
   :component-pathname :component-name :perform :component-depends-on
                :compile-op :load-op
                :make-operation)
  (:import-from :uiop :run-program)
  (:export :org-source-file :tangle-op))
(cl:in-package :asdf-org-babel-tangle/main)

(defclass org-source-file (cl-source-file)
  ()
  (:documentation "component class for an org-mode literate text file that can tangled into a common lisp source file"))

(defclass tangle-op (sideway-operation upward-operation)
  ()
  (:documentation "use org-babel-tangle to convert an org-source-file into a cl-source-file"))

(defmethod input-files ((op compile-op) (file org-source-file))
  (output-files (make-operation 'tangle-op) file))

(defmethod input-files ((op tangle-op) (file org-source-file))
  (list (merge-pathnames (make-pathname :type "org")
                         (component-pathname file))))

(defmethod output-files ((op tangle-op) (file org-source-file))
  (list (component-pathname file)))

(defun tangle (source-path output-path)
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
