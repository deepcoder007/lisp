

(defmacro my-and (&rest exprs)
  (if (= (length exprs) 1)
      (first exprs)
      `(if ,(first exprs)
           ,(first exprs)
           (my-and ,@(rest exprs)))))
