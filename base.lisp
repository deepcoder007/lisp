;; First example of lisp code



(defun foo (&key a b c) 
  (list a b c))


(defmacro when_macro (condition &rest body)
  `(if ,condition (progn ,@body))
  )


(defmacro print_code (condition &rest body)
  `(progn (print "BEGIN_M")
          (print (quote ,condition))
          (print "END_M"))
  )
