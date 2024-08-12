;; First example of lisp code

(defun foo (&key a b c)
  (list a b c))

(defmacro when_macro (condition &rest body)
  `(if ,condition (progn ,@body))
  )

(defmacro print_code (condition &rest body)
  `(progn (print "BEGIN_COND")
          (print ,condition)
          (print (quote ,condition))
          (print "END_COND")
          (print "BEGIN_BODY")
          (print (quote ,@body))
          (print "END_BODY")
          )
  )

;; (progn (print '(a (+ 1 2) c))
;;        (print '(a ,(+ 1 2) c))
;;        (print '(a (list 1 2) c))
;;        (print '(a ,(list 1 2) c))
;;        (print '(a ,@(list 1 2) c))
;;        )

;; (print
;; (macroexpand-1
;;  '(print_code (> 123 34) (print "message"))))


(defmacro test-gensym (&rest body)
  `(progn (print "BEGIN_MACRO")
          (gensym)
          (print "END_MACRO")
          (print "BEGIN_BODY")
          (print (quote ,@body))
          (print "END_BODY")
          )
  )

(print
 (macroexpand-1
  '(test-gensym (> 123 34) (1 2 3))))

(print_code (> 1 10)  (+ 5 10))

;; (defmacro test-gensym-2 (&rest body)
(defmacro test-gensym-2 ()
  (progn (print "BEGIN_MACRO")
         (print "END_BODY")
         (print "END_BODY-")
         )
  )

(print "== gensym ====================")

(print
 (macroexpand-1
  '(test-gensym-2)))

(print "=== For loop 1 ======================")

(loop for x in '(1 2 3 4 5)
      do (print x))

(print "=== For loop 2 ======================")

(loop for x from 10 to 13
      do (print (+ x 9999)))

(print "BIG Line break ======================")

(defmacro repeat-body (n &body body)
  (loop for x from 1 to n
        collect `(progn (print "hell***")
                        ,@body
                        (print "second**"))))

;; (repeat-body 4 (print "hello World"))

;;(defmacro repeat-body (n &body body)
;;  (loop for x from 1 to n
;;        collect '((print "second**")
;;                  (print "first"))))

(defmacro repeat-body (n &body body)
  (loop for x from 1 to n
        collect `(,@body)))

(print "macro repeat-body expand ======================")

;; (repeat-body 5 (progn (print 1) (print 2)))


(print
 (macroexpand-1
  '(repeat-body 5 (progn (print 1) (print 2)))))


(print "------ vector ----------")

(defparameter *v* (vector 1 2 3 4))

(defparameter *x* (make-array 5 :fill-pointer 0))

(setf (elt *v* 0) 10)

(print "======= LIST PROCESSING ==========")

(car '(1 2 3))
(cdr '(1 2 3))

(print "====== CLOS examples ====== ")

(defgeneric draw (shape)
  (:documentation "Draw the given shape on the screen."))


(defclass circle ()
  ((v1
    :initarg :v1
    :reader get_v1)
   (v2
    :initarg :v2
    :reader get_v2)))

(defclass triangle ()
  (v3 v4))


(defparameter *c1* (make-instance 'circle :v1 "value 1" :v2 "value 2"))
(defparameter *c2* (make-instance 'circle :v1 "another v1" :v2 "another value 2"))

(defparameter *t1* (make-instance 'triangle))
(defparameter *t2* (make-instance 'triangle))


(slot-value *c1* 'v1)

(get_v1 *c1*)

(defmethod draw ((shape circle))
  (print "Draing a circle"))


(defmethod draw ((shape triangle))
  (print "Drawing a triangle"))

(block sample-block
  (print "Start block")
  (print "v1")
  (print "v1")
  (return-from sample-block)
  (print "v1")
  (print "v1")
  (print "v2"))


;; (tagbody sample-tag
;;    (print "Start block")
;;    (print "v1")
;;    (print "v1")
;;    (go sample-tag)
;;    (print "v1")
;;    (print "v1")
;;    (print "v2"))


(flet ((test-flet ()
         (print "test-flet called"))
       (print-n (n)
         (print n))
       )
  (test-flet)
  (print-n "asdfasdf")
  (test-flet)
  (print-n "asdfasdf")
  (test-flet)
  (print-n 34343)
  (test-flet))

(symbol-name :keyword-example)


(defun hello-world ()
  (print "HEllo World normal"))

(defmacro append-list-test (l1 l2)
  `(quote ,@l1 ,@l2))


(append-list-test (1 2 3) (4 5 6))

(macroexpand-1
 `(append-list-test (1 2 3) (4 5 6)))


(defun fibonac-macro (x)
  "Calculate fibonacci number"
  (cond ((eq x 0) 1)
        ((eq x 1) 1)
        ((> x 1) (+ (fibonac-macro (- x 1)) (fibonac-macro (- x 2))))))

;; Example of emacs package


(defmacro custom-in (obj &rest choices)
  `(or ,@(mapcar #'(lambda (c) `(eql ,obj ,c)) choices)))


(defmacro custom-in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices))
       )
    )
  )


(custom-in 1 '(1 2 3))

(custom-in 1 1 2 3)

(mapcar #'(lambda (x) (print (+ 101 x))) '(1 2 3 4 5 6))

(mapcar #'(lambda (x) `(print (+ 101 x))) '(1 2 3 4 5 6))


(defparameter *some-list* (list* 'one 'two 'three 'four))

(list* 'one 'two 'three 'four 'five 'six)

(let ((x (list* 1 2 3 4 5 6 7)))
  (print (car x)) (print (cdr x)))


(let ((x (list 1 2 3 4 5 6 7)))
  (print (car x)) (print (cdr x)))


(defparameter *some-list* (list 'one 'two 'three 'four))


(define-modify-macro toggle () not)


(defparameter *tmp-val* '(NIL 2))
(toggle (car *tmp-val*))


(print "---------- on Lisp page 190 --------------")

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))


(aif (> 15  10) (print it))



(aif (> (progn (print 15) 15)  10) (print it))


(do ((i 0 (+ 3 i)))
    ((> i 20))
  (print i))

(member-if #'null '(1 nil 6))


(ignore-errors
 (/ 3 0))


(ignore-errors
 (/ 31 10))


(handler-case (/ 3 0)
  (error (c)
    (print "HAHAAHAH")
    (values 0 c)))


(handler-case (/ 3 0)
  (division-by-zero (c)
    (print "HAHAAHAH")
    (values 0 c)))


(defun our-every (fn lst)
  (if (null lst)
      t
      (and (funcall fn (car lst))
           (our-every fn (cdr lst)))))

(our-every #'(lambda (x)
               (print (+ 110 x)))
           '(1 2 3 4))


(our-every #'(lambda (x)
               (print (+ 110 x)))
           '(1 2 3 4))

(loop for x in '(1 2 3 4 5)
      do (print x))

(loop for x from 10 to 13
      do (print (+ x 9999)))


(defmacro custom_loop_n (N &rest body)
  `(progn (print "BEGIN")
          (loop for x from 1 to ,N
                do (print x))
          (print "END")))

(defmacro custom_loop_5 (body)
  `(progn (print "BEGIN")
         (loop for x from 1 to 5
               do (progn (print x)))
         (print "END")))

`(custom_loop_5 (print 10))

(custom_loop_n 5 (print 1))


(custom_loop_5 (1 print))

(macroexpand-1 `(custom_loop_5
                 (print 1)))


(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(abbrev add +)

(add 2 3)

(add 2 3)


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

(alist-expand 10 (* it .05) (* it 3))

(alist 10 (* it .05) (* it 3))


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

(call_fn_N 4 (print 10))

(call_fn_N 4 (print 10))


(call_fn_N 4 '(10))


(call_fn_N 1 10)

(call_fn_N 1 10)

(call_fn_N 2 10)

(call_fn_N 2 10)

(call_fn_N 3 10)

(call_fn_N 3 '(10))

;; some tests

(append (append nil '(print 10)) '(print 20))




(print "BIG Line break ======================")
(print
 (macroexpand-1
  '(test-gensym (> 123 34) (1 2 3))))

(print_code (> 1 10)  (+ 5 10))

;; (defmacro test-gensym-2 (&rest body)
(defmacro test-gensym-2 ()
  (progn (print "BEGIN_MACRO")
         (print "END_BODY")
         (print "END_BODY-")
         )
  )

(print "== gensym ====================")

(print
 (macroexpand-1
  '(test-gensym-2)))

(print "=== For loop 1 ======================")

(loop for x in '(1 2 3 4 5)
      do (print x))

(print "=== For loop 2 ======================")

(loop for x from 10 to 13
      do (print (+ x 9999)))

(print "BIG Line break ======================")

(defmacro repeat-body (n &body body)
  (loop for x from 1 to n
        collect `(progn (print "hell***")
                        ,@body
                        (print "second**"))))

;; (repeat-body 4 (print "hello World"))

;;(defmacro repeat-body (n &body body)
;;  (loop for x from 1 to n
;;        collect '((print "second**")
;;                  (print "first"))))

(defmacro repeat-body (n &body body)
  (loop for x from 1 to n
        collect `(,@body)))

(print "macro repeat-body expand ======================")

;; (repeat-body 5 (progn (print 1) (print 2)))


(print
 (macroexpand-1
  '(repeat-body 5 (progn (print 1) (print 2)))))


(print "------ vector ----------")

(defparameter *v* (vector 1 2 3 4))

(defparameter *x* (make-array 5 :fill-pointer 0))

(setf (elt *v* 0) 10)

(print "======= LIST PROCESSING ==========")

(car '(1 2 3))
(cdr '(1 2 3))

(print "====== CLOS examples ====== ")

(defgeneric draw (shape)
  (:documentation "Draw the given shape on the screen."))


(defclass circle ()
  ((v1
    :initarg :v1
    :reader get_v1)
   (v2
    :initarg :v2
    :reader get_v2)))

(defclass triangle ()
  (v3 v4))


(defparameter *c1* (make-instance 'circle :v1 "value 1" :v2 "value 2"))
(defparameter *c2* (make-instance 'circle :v1 "another v1" :v2 "another value 2"))

(defparameter *t1* (make-instance 'triangle))
(defparameter *t2* (make-instance 'triangle))


(slot-value *c1* 'v1)

(get_v1 *c1*)

(defmethod draw ((shape circle))
  (print "Draing a circle"))


(defmethod draw ((shape triangle))
  (print "Drawing a triangle"))

(block sample-block
  (print "Start block")
  (print "v1")
  (print "v1")
  (return-from sample-block)
  (print "v1")
  (print "v1")
  (print "v2"))


;; (tagbody sample-tag
;;    (print "Start block")
;;    (print "v1")
;;    (print "v1")
;;    (go sample-tag)
;;    (print "v1")
;;    (print "v1")
;;    (print "v2"))


(flet ((test-flet ()
         (print "test-flet called"))
       (print-n (n)
         (print n))
       )
  (test-flet)
  (print-n "asdfasdf")
  (test-flet)
  (print-n "asdfasdf")
  (test-flet)
  (print-n 34343)
  (test-flet))

(symbol-name :keyword-example)


(defun hello-world ()
  (print "HEllo World normal"))

(defmacro append-list-test (l1 l2)
  `(quote ,@l1 ,@l2))


(append-list-test (1 2 3) (4 5 6))

(macroexpand-1
 `(append-list-test (1 2 3) (4 5 6)))


(defun fibonac-macro (x)
  "Calculate fibonacci number"
  (cond ((eq x 0) 1)
        ((eq x 1) 1)
        ((> x 1) (+ (fibonac-macro (- x 1)) (fibonac-macro (- x 2))))))

;; Example of emacs package


(defmacro custom-in (obj &rest choices)
  `(or ,@(mapcar #'(lambda (c) `(eql ,obj ,c)) choices)))


(defmacro custom-in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices))
       )
    )
  )


(custom-in 1 '(1 2 3))

(custom-in 1 1 2 3)

(mapcar #'(lambda (x) (print (+ 101 x))) '(1 2 3 4 5 6))

(mapcar #'(lambda (x) `(print (+ 101 x))) '(1 2 3 4 5 6))


(defparameter *some-list* (list* 'one 'two 'three 'four))

(list* 'one 'two 'three 'four 'five 'six)

(let ((x (list* 1 2 3 4 5 6 7)))
  (print (car x)) (print (cdr x)))


(let ((x (list 1 2 3 4 5 6 7)))
  (print (car x)) (print (cdr x)))


(defparameter *some-list* (list 'one 'two 'three 'four))


(define-modify-macro toggle () not)


(defparameter *tmp-val* '(NIL 2))
(toggle (car *tmp-val*))


(print "---------- on Lisp page 190 --------------")

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))


(aif (> 15  10) (print it))



(aif (> (progn (print 15) 15)  10) (print it))


(do ((i 0 (+ 3 i)))
    ((> i 20))
  (print i))

(member-if #'null '(1 nil 6))


(ignore-errors
 (/ 3 0))


(ignore-errors
 (/ 31 10))


(handler-case (/ 3 0)
  (error (c)
    (print "HAHAAHAH")
    (values 0 c)))


(handler-case (/ 3 0)
  (division-by-zero (c)
    (print "HAHAAHAH")
    (values 0 c)))


(defun our-every (fn lst)
  (if (null lst)
      t
      (and (funcall fn (car lst))
           (our-every fn (cdr lst)))))

(our-every #'(lambda (x)
               (print (+ 110 x)))
           '(1 2 3 4))


(our-every #'(lambda (x)
               (print (+ 110 x)))
           '(1 2 3 4))

(loop for x in '(1 2 3 4 5)
      do (print x))

(loop for x from 10 to 13
      do (print (+ x 9999)))


(defmacro custom_loop_n (N &rest body)
  `(progn (print "BEGIN")
          (loop for x from 1 to ,N
                do (print x))
          (print "END")))

(defmacro custom_loop_5 (body)
  (progn (print "BEGIN")
         (loop for x from 1 to 5
               do (progn (print x)))
         (print "END")))


(custom_loop_n 5 (print 1))


(custom_loop_5 (1 print))

(macroexpand-1 `(custom_loop_5
                 (print 1)))


(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(abbrev add +)

(add 2 3)

(add 2 3)


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

(alist-expand 10 (* it .05) (* it 3))

(alist 10 (* it .05) (* it 3))


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

(call_fn_N 4 (print 10))

(call_fn_N 4 (print 10))


(call_fn_N 4 '(10))


(call_fn_N 1 10)

(call_fn_N 1 10)

(call_fn_N 2 10)

(call_fn_N 2 10)

(call_fn_N 3 10)

(call_fn_N 3 '(10))

;; some tests

(append (append nil '(print 10)) '(print 20))




(print "BIG Line break ======================")
