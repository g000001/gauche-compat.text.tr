;;;; gauche-compat.text.tr.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :gauche-compat.text.tr
  :serial t
  :depends-on (:fiveam
               :srfi-1
               :srfi-5
               :srfi-8
               :srfi-11)
  :components ((:file "package")
               (:file "util")
               (:file "gauche-compat.text.tr")
               (:file "test-tr")))

(defmethod perform ((o test-op) (c (eql (find-system :gauche-compat.text.tr))))
  (load-system :gauche-compat.text.tr)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :gauche-compat.text.tr.internal :gauche-compat.text.tr))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
