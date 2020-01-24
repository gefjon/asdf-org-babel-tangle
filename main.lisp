(uiop:define-package :asdf-org-babel-tangle/main
    (:use :cl)
  (:nicknames :asdf-org-babel-tangle)
  (:import-from :asdf
   :cl-source-file :operation :input-files :output-files
   :component-name :perform :component-depends-on :compile-op :load-op)
  (:import-from :uiop :run-program)
  (:export :org-source-file :tangle-op))
(cl:in-package :asdf-org-babel-tangle/main)

(defclass org-source-file (cl-source-file)
  ()
  (:documentation "component class for an org-mode literate text file that can tangled into a common lisp source file"))

(defclass tangle-op (operation)
  ()
  (:documentation "use org-babel-tangle to convert an org-source-file into a cl-source-file"))

(defmethod input-files ((op tangle-op) (file org-source-file))
  (list (concatenate 'string (component-name file) ".org")))

(defmethod output-files ((op tangle-op) (file org-source-file))
  (list (concatenate 'string (component-name file) ".lisp")))

(defmethod perform ((op tangle-op) (file org-source-file))
  (let* ((input-file (first (input-files op file)))
         (output-file (first (output-files op file)))
         (babel-tangle-arg-source-lang "lisp")
         (command (format nil
                          "emacs -batch ~s --eval '(require 'ob-tangle) (org-babel-tangle nil ~s ~s)"
                          input-file
                          output-file
                          babel-tangle-arg-source-lang)))
    (run-program command)))

(defmethod component-depends-on ((op compile-op) (file org-source-file))
  `((tangle-op ,(component-name file))
    ,@(call-next-method)))

(defmethod component-depends-on ((op load-op) (file org-source-file))
  `((tangle-op ,(component-name file))
    ,@(call-next-method)))
