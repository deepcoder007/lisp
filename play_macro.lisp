

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

(defun flatten (x)
  (labels ((rec (x acc)
                (cond ((null x) acc)
                      ((typep x 'sb-impl::comma) (rec (sb-impl::comma-expr x) acc))
                      ((atom x) (cons x acc))
                      (t (rec
                            (car x)
                            (rec (cdr x) acc))))))
    (rec x nil)))

(flatten '(1 (2 3) 4))

(flatten '(1 (2 3 (4 5 6)) 4))

  (defun g!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "G!"
                  :start1 0
                  :end1 2)))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
      `(defmacro ,name ,args
         (let ,(mapcar
                (lambda (s)
                  `(,s (gensym ,(subseq
                                 (symbol-name s)
                                 2))))
                syms)
           ,@body))))


(defmacro/g! test_macro (v)
  `(progn
     (print ,v)
     (setf ,g!v 101)
     (print ,g!v)
     (setf ,g!result 1010)
     (print ,g!result)
     ))

(test_macro 10)

;; gensym done in inner invocation of defmacro/g!
(defmacro/g! junk-outer ()
  `(defmacro/g! junk-inner ()
     `(let ((,g!abc))
        ,g!abc)))

;; gensym done in outer invocation of defmacro/g!
(defmacro/g! junk-outer ()
  `(defmacro/g! junk-inner ()
     `(let ((,,g!abc))
        ,,g!abc)))

(junk-outer)

(defun o!-symbol-p (s)
  (and (symbolp s)
        (> (length (symbol-name s)) 2)
        (string= (symbol-name s)
                "O!"
                :start1 0
                :end1 2)))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun o!-symbol-to-g!-symbol (s)
  (symb "G!"
        (subseq (symbol-name s) 2)))

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p (flatten args)))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
      `(defmacro/g! ,name ,args
         `(let ,(mapcar #'list (list ,@gs) (list ,@os))
            ,(progn ,@body)))))


(defmacro! square (o!x)
  `(* ,g!x ,g!x))

(defmacro! osquare (o!x)
  `(* ,o!x ,o!x))

(square 5)

(square (progn
          (print 10)
          5))

(defun let-binding-transform (bs)
  (if bs
    (cons
      (cond ((symbolp (car bs))
              (list (car bs)))
            ((consp (car bs))
              (car bs))
            (t
              (error "Bad let bindings")))
      (let-binding-transform (cdr bs)))))


; alet-hotpatch
(defmacro alet-hotpatch% (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest args)
       (if (eq (car args) ':hotpatch)
           (setq this (cadr args))
           (apply this args)))))

(setf (symbol-function 'hotpatch-test)
      (alet-hotpatch% ((acc 0))
                      (lambda (n)
                        (incf acc n))))

(alet-hotpatch% ((acc 0))
                (lambda (n)
                  (incf acc n)))

(defmacro! dlambda (&rest ds)
  `(lambda (&rest ,g!args)
     (case (car ,g!args)
       ,@(mapcar
           (lambda (d)
             `(,(if (eq t (car d))
                  t
                  (list (car d)))
               (apply (lambda ,@(cdr d))
                      ,(if (eq t (car d))
                         g!args
                         `(cdr ,g!args)))))
           ds))))

(defmacro! let-hotpatch (letargs &rest body)
  `(let ((,g!this) ,@letargs)
     (setq ,g!this ,@(last body))
     ,@(butlast body)
     (dlambda
      (:hotpatch (closure)
                 (setq ,g!this closure))
      (t (&rest args)
         (apply ,g!this args)))))


(dlambda
  (:hotpatch (closure)
              (setq 'test-sym closure))
  (:coldpathc (closure)
              (setq 'second-test-sym closure))
  (t (&rest args)
      (apply 'test-sym args)))

(dlambda
(:hotpatch (closure)
            (setq ,g!this closure))
(t (&rest args)
    (apply ,g!this args)))

;; decipher dlambda
(defmacro! debug_dlambda (&rest ds)
  (mapcar (lambda (d)
            `(print ,(cdr d)))
          ds))

(debug_dlambda
  (:hotpatch (closure)
              (setq 'test-sym closure))
  (t (&rest args)
      (apply 'test-sym args)))


(debug_dlambda
  (:hotpatch (closure)
              (setq 'test-sym closure))
  (t (&rest args)
      (apply 'test-sym args)))
