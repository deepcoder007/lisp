

(print "test read-macro")


(defparameter sample-fn
  (lambda () (print "This is a lambda function")))

(compile 'compiled-sample-fn
         (lambda () (print "This is a lambda function")))


(defun fib (x)
  (cond
    ((> x 1)
     (+ (fib (- x 1)) (fib (- x 2))))
    ((or (= x 0) (= x 1))
     1)
    ))

'(loop for x from 1 to 40
  do (print (time (fib x))))

'(loop for x from 30 to 35
  do (print (time (fib x))))

;; fix this recursive fib macro
'(defmacro fib-macro (x)
  (cond
    (`(> ,x 1)
      `(+ (fib-macro (- ,x 1)) (fib-macro (- ,x 2))))
    ((or `(= ,x 0) `(= ,x 1))
     1)
    ))

'(fib-macro 0)


(eval (list 'cons '1 '2))

(eval (list '+ '1 '2 '3 '4))

;; evaluating a list calculated from scratch
(eval (list '+ '1 '2 '3 '4))


(eval-when (:compile-toplevel) (print "compile evaluation"))

(eval-when (:load-toplevel) (print "load evaluation"))

(eval-when (:execute) (print "execute evaluation"))


(defun test-fn ()
  (progn
    (eval-when (:compile-toplevel) (print "compile evaluation fn"))
    (eval-when (:load-toplevel) (print "load evaluation fn"))
    (eval-when (:execute) (print "execute evaluation fn"))
    ))

'(print "conflicted region")


(defun single-quote-reader (stream char)
  (declare (ignore char))
  (print "BEGIN reading")
  (list (quote quote) (read stream t nil t))
  (print stream)
  (print "END reading"))

(set-macro-character #\* #'single-quote-reader)


(eval-when (:execute) (print "hello World"))

(print "hello world")

(second `(1 2 3 4 5))

(mapcar (lambda (a) '(,a 'empty)) '(var-a var-b var-c))

(mapcar (lambda (a) `(,a 'empty)) '(var-a var-b var-c))

(mapcar #`(,a1 'empty) '(var-a var-b var-c))

;;;;;;;;;;;;;;;;;
;;;;  some more lambda programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro alet% (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     this))


(alet% ((sum) (mul) (expt))
       (funcall this)
       (lambda () (print "another level of function"))
       (lambda () (print "hello world")))


(alet% ((sum) (mul) (expt))
       (funcall this)
       (lambda () (print "another level of function"))
       (lambda () (print "another level of function"))
       (lambda () (print "another level of function"))
       (lambda () (print "another level of function"))
       (lambda () (print "another level of function"))
       (lambda () (print "another level of function"))
       (lambda () (print "another level of function"))
       (lambda () (print "hello world")))

(eval-when (:compile-toplevel) (print "hello world"))

(defun single-quote-reader (stream char)
  (declare (ignore char))
  (print "BEGIN reading")
  (list (quote quote) (read stream t nil t))
  (print stream)
  (print "END reading"))

(set-macro-character #\* #'single-quote-reader)


(eval-when (:execute) (print "hello World"))

(print "hello world")

(second `(1 2 3 4 5))

(mapcar (lambda (a) '(,a 'empty)) '(var-a var-b var-c))

(mapcar (lambda (a) `(,a 'empty)) '(var-a var-b var-c))

(mapcar #`(,a1 'empty) '(var-a var-b var-c))

;;;;;;;;;;;;;;;;;
;;;;  some more lambda programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro alet% (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     this))


(alet% ((sum) (mul) (expt))
       (funcall this)
       (lambda () (print "another level of function"))
       (lambda () (print "hello world")))


(alet% ((sum) (mul) (expt))
       (funcall this)
       (lambda () (print "another level of function"))
       (lambda () (print "another level of function"))
       (lambda () (print "another level of function"))
       (lambda () (print "another level of function"))
       (lambda () (print "another level of function"))
       (lambda () (print "another level of function"))
       (lambda () (print "another level of function"))
       (lambda () (print "hello world")))
