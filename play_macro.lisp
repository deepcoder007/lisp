

(defmacro my-and (&rest exprs)
  (if (= (length exprs) 1)
      (first exprs)
      `(if ,(first exprs)
           ,(first exprs)
           (my-and ,@(rest exprs)))))

(defmacro my-and-v2 (&rest exprs)
  (if (= (length exprs) 1)
      `(progn
        (first exprs)
        (if ,(first exprs)
            ,(first exprs)
            (my-and-v2 ,@(rest exprs))))))

(defmacro my-and-v3 (&rest exprs)
  `(print (cddr (quote ,exprs)))
  )

(my-and-v3 (print 1) (print 2) (print 3) (print 4))

(my-and-v3 (print 1) (print 2) (print 3) (print 4))

;;   Saved args
(my-and (print 1) (print 2) (print 3) (print 4))

(my-and-v2 (print 1) (print 2) (print 3) (print 4))

(my-and-v3 (print 1) (print 2) (print 3) (print 4))

(defmacro my-and-recursive-v1 (&rest exprs)
  `(print (cddr (quote ,exprs)))
  )

(my-and-recursive-v1 (print 1) (print 2) (print 3) (print 4))
