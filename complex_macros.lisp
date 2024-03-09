
(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(abbrev add +)

(add 2 3)

(add 2 30)

;;;  book example

(defun a+expand (args syms)
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           ,(a+expand (cdr args)
                      (append syms (list sym)))))
      `(+ ,@syms)))

(defmacro a+ (&rest args)
  (a+expand args nil))


(defmacro alist (&rest args)
  (alist-expand args nil))

(defun alist-expand (args syms)
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           ,(alist-expand (cdr args)
                          (append syms (list sym)))))
      `(list ,@syms)))


(defun mass-cost (menu-price)
  (a+ menu-price (* it .05) (* it 3)))

(mass-cost 10)

(a+ 10 (* it .05) (* it 3))

(alist 10 (* it .05) (* it 3))


;; custom macros

`(defun call_fn (N accum &rest args)
   (if (> N 0)
       `(,@args ,@(call_fn (- N 1) nil args))
       )
   )

(defun call_fn (N accum &rest args)
  (if (> N 0)
      `(,@(call_fn (- N 1) `(append ,accum ,@args) (car args)))
      accum
      )
  )

(defmacro call_fn_N (N args)
  (call_fn N nil args))

(call_fn_N 4 '(10))

(call_fn_N 1 10)

(call_fn_N 1 10)

(call_fn_N 2 10)

(call_fn_N 2 10)

(call_fn_N 3 10)

(call_fn_N 3 '(10))

;; some tests


(defparameter *some_list* '(1 2 3 4 5))

(print *some_list*)
(setf (car *some_list*) '(print 10))
(setf (car (cdr *some_list*)) '(print 10))


(defparameter *raw_list* '(1 2 3 4 5))

(append  (mapcar #'(lambda (x) '(+ 100 x)) *raw_list*))

(reduce #'+ (mapcar #'(lambda (x) (+ 100 x)) *raw_list*))

(reduce #'list '(1 2 3 4 5) :initial-value '(9 8))
(reduce #'list '(1 2 3 4 5) :initial-value 9)
(reduce #'list  (reverse '(1 2 3 4 5)))


(defun test-reduce (x y)
  (progn (print x) (print y) (list x y)))

(reduce #'test-reduce '(1 2 3 4 5))



(defmacro add-quote (&rest args)
  `(quote (car (quote ,@args))))

(add-quote (print 1) (print 2) (print 3))


(defun add-quote (st &rest args)
  (if args
      `(,(add-quote (append st (car args)) (cdr args)))
      `(quote ,st)))


(defun process-record-fn (st &rest args)
  (if args
      `(,@(process-record-fn st (cdr args)))
      `(print "last entry")))


(defmacro process-record-macro (&rest args)
  (process-record-fn '() args))


(process-record-macro (print 1) (print 2))


(defmacro count-args (&rest args)
  (print (length args)))

(count-args 1 2 3 4 5 6)

(count-args (print 1) (print 2) (print 3) (print 10))


(defmacro eval-first (&rest args)
  (if (length args)
      (print "skip")
      (print "done")))

(defmacro eval-first (&rest args)
  (if (length args)
      (progn (print "BEGIN element")
             `(mapcar #'append
               (quote ,(cdr args)))
             )
      (print "done")))


(eval-first (print 111) (print 2) (print 3))

(eval-first (print 111) (print 2) (print 3))

(eval-first (print 111) (print 2) (print 3) (print 4))
