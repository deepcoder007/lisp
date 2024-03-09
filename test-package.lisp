
(defpackage :test-package
  (:use :common-lisp)
  (:export :hello-world))

(in-package :test-package)


(defun hello-world ()
  (print "hello world in test-package"))
