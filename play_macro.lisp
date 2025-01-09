

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

(defmacro arg-entry (&rest exprs)
  `(cdr (quote ,exprs)))

(arg-entry (print 1) (print 2) (print 3) (print 4))

;;
;; testing of recursive macros
;;
(defmacro my-and-recursive-v1 (level &rest exprs)
  (if (> (length exprs) 1)
    `(progn (print "before eval")
          (eval (car (quote ,exprs)))
          (format t "---- print rest ~d\n" ,level)
          (mapcar #'eval (quote ,(cdr exprs)))
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

;;;
;;; Counter macro
;;;
(defmacro gen-number-seq (begin end)
  (if (< begin end)
      `(progn
         (print ,begin)
         (gen-number-seq ,(+ begin 1) ,end)
         )
      ))

(gen-number-seq 10 20)

(gen-number-seq 21 20)

(macroexpand-1 '(gen-number-seq 10 20))


(defmacro cond-macro (begin end)
  (if (< begin end)
    (progn
        (print `(,begin ,end))
        `(cond-macro ,(+ begin 1) ,end)
        )))

(cond-macro 10 20)

(defmacro expand-range (begin end)
  (if (< begin end)
    (progn
        (print `(,begin ,end))
        `(expand-range ,(+ begin 1) ,end)
        `(cons ,begin (expand-range ,(+ begin 1) ,end))
        )
    ()))


(expand-range 10 15)


(defmacro slow-expand-range (begin end)
  (if (< begin end)
    `(cons ,begin (slow-expand-range ,(+ begin 1) ,end))
    '()))

(slow-expand-range 17 20)

;; dlambda
(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(defmacro aif (test then &optional else)
  `(let ((it, test))
     (if it ,then ,else)))

(defmacro alet% (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     this))


(alambda (n)
         (if (> n 0)
             (cons
              n
              (self (- n 1)))))


(alet% ((sum) (mul) (expt))
       (print 1)
       (print this)
       (print 3)
       (print 4)
       )

(defmacro alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

(alet ((acc 0))
      (alambda (n)
        (if (eq n 'invert)
            (setq this
                  (lambda (n)
                    (if (eq n 'invert)
                        (setq this #'self)
                        (decf acc n))))
            (incf acc n))))


(defmacro alet-fsm (&rest states)
  `(macrolet ((state (s)
                `(setq this #',s)))
     (labels (,@states) #',(caar states))))

(alet ((acc 0))
      (alet-fsm
         (going-up (n)
                (if (eq n 'invert)
                    (state going-down)
                    (incf acc n)))
         (going-down (n)
                (if (eq n 'invert)
                    (state going-up)
                    (decf acc n)))))

(alet ((acc 0))
      (alet-fsm
         (going-up (n)
                (if (eq n 'invert)
                    (state going-down)
                    (incf acc n)))
         (going-down (n)
                (if (eq n 'invert)
                    (state going-up)
                    (decf acc n)))))
