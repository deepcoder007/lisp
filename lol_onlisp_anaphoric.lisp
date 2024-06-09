
;; simlify recursion via macro
(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))


(alambda (n)
  (if (> n 0)
      (cons
       n
       (self (- n 1)))))

(alambda 5)

(alambda 5)



;; sharp backquote

(mapcar (lambda (a)  (list a ''empty))
        '(var-a var-b var-c))


(mapcar (lambda (a)  `(list ,a 'empty))
        '(var-a var-b var-c))
