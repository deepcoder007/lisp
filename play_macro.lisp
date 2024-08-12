

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

;;
;; testing on different argument lengths and opertions over those
;;
(defmacro arg-length-m (&rest exprs)
  (length exprs))

(arg-length-m (print 1) (print 2) (print 3) (print 4))

(defmacro arg-length-m2 (&rest exprs)
  `(length (quote ,exprs)))

(arg-length-m2 (print 1) (print 2) (print 3) (print 4))


(defmacro arg-length-m3 (&rest exprs)
  `(mapcar 'eval (quote ,exprs)))

(arg-length-m3 (print 1) (print 2) (print 3) (print 4))

(defmacro arg-length-m4 (&rest exprs)
  `(mapcar 'eval (cdr (quote ,exprs))))

(arg-length-m4 (print 1) (print 2) (print 3) (print 4))

;;
;; testing of recursive macros
;;
(defmacro my-and-recursive-v1 (level &rest exprs)
  (if (> (length exprs) 1)
    `(progn (print "before eval")
          (eval (car (quote ,exprs)))
          (format t "---- print rest ~d\n" ,level)
          (mapcar #'eval (cdr (quote ,exprs)))
          (format t " ----- recursive call begin ~d\n" ,level)
          (my-and-recursive-v1 ,(+ level 1) ,@(cdr exprs))
          (format t " ----- recursive call end ~d\n" ,level)
          (print "after eval")
    )
    `(progn (print "too small input")
           (print (quote ,exprs)))
    ))

(my-and-recursive-v1 0 (print 1) (print 2) (print 3) (print 4))

(my-and-recursive-v1 (print "hello") (print "hello2"))
