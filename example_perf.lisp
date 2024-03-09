;;
;;

(defun return-2x-fn (x)
  (+ 2 x))


(defmacro return-2x-macro (x)
  (+ 2 x))


(time (return-2x-fn 10))

(time (return-2x-macro 10))


(time (loop for x from 1 to 1000
            collect (return-2x-fn x)))


(defmacro loop-macro (start end mac)
  (if (< `(eval ,start) end)
      `(progn (print "==== BEGIN MACRO ====="))
      `(progn (print "last batch"))))


(loop-macro 1 10 10101)

(loop-macro 10 10 10101)

(loop-macro (+ 1 2) 10 10101)


(defun factor-fn (x)
  (if (zerop x)
      1
      (* (factor-fn (- x 1)) x)))

(print "=====================================")
(loop for x from 5 to 10
      do (time (factor-fn x)))

(defun prev (x)
  (- x 1))


(defmacro factor-macro (x)
  (if (zerop x)
      1
      (* (factor-macro `(prev ,x)) x)))

(print "=====================================")
(factor-macro 10)


(defmacro nlet (n letargs &rest body)
  `(labels ((,n ,(mapcar #'car letargs)
              ,@body))
     (,n ,@(mapcar #'cadr letargs))))

(defun nlet-fact (n)
  (nlet fact ((n n))
        (if (zerop n)
            1
            (* n (fact (- n 1))))
        ))


;; labels example
(defun recursive-times (k n)
  (labels ((temp (n)
             (if (zerop n)
                 0
                 (+ k (temp (- n 1)))))
           )
    (temp n)))

(nlet fact ((n n))
      (if (zerop n)
          1
          (* n (fact (- n 1))))
      )


(nlet fact ((n 10))
      (if (zerop n)
          1
          (* n (fact (- n 1))))
      )

;; g-bang symbosl
