

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x)
                           (rec (cdr x) acc))))))
    (rec x nil)))


(flatten '(1 2 3))
 ; => (1 2 3)



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


(defmacro test_c_macro (name args &rest body)
  (let ((syms (flatten body)))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(subseq
                               (symbol-name s)
                               2))))
              syms)
         ,@body))))


(defmacro this_macro (&rest args)
  (let ((fargs (flatten args)))
               (if (< 3 (length args))
                   `(+ ,@fargs)
                   `(print "smaller then 2"))))

(this_macro 1 2 (3 4 5) (6 7 (8 9 10)))
 ; => 55 (6 bits, #x37, #o67, #b110111)

(this_macro 1 2)
; "smaller then 2"  => "smaller then 2"

'(mapcar (lambda (x)
            (progn
                (print ,(symbol-name ,x))
                (print ,x)))
                args)

                (mapcar (lambda (x)
                            (progn
                               (print x)))
                                arg_list)

(defmacro test_c_macro (name args &rest body)
  (let ((syms (flatten body)))
     (print "expanding outer macro")
    `(defmacro ,name (&rest arg_list)
          (print "expanding inner macro")
          (let ((args ',args))
              `(progn (print "executing inner macro")
                      (mapcar (lambda (x) (print x)) (quote ,arg_list))
                      (mapcar (lambda (x)
                                  (progn
                                      '(print (symbol-name x))
                                      (print x)))
                                      (mapcar (lambda (y) (symbol-name y)) ',args))
                      (print "before value")
                      (print "after value")
                      )))))


(test_c_macro this_macro (a b c d)
              (+ (a b)))

(this_macro 1 2 3 4 5 6 7 8 9 10)

(defmacro test_d_macro (name args &rest body)
  (let ((syms (flatten body))
        (mapcar (lambda (x) (symbol-name x)) '(s1 s2 s3 s4)))
     (print "expanding outer macro")
    `(defmacro ,name (&rest arg_list)
          (progn
            (print "expanding inner macro")
            '(print ,syms)
            ,body
          ))))

(defmacro test_d_macro (name args &rest body)
  (let ((a 1)
        (b 2)
        (c 3)
        (d 4)
        (e 5))
    `(defmacro ,name ()
       (let ,(mapcar (lambda (x) `(,x ,(symbol-name x))) args)
        `(progn
              (print ,a)
              (print ,b)
              (print ,c)
              ,(mapcar (lambda (x) `(print ,x)) ',args)
              ,(mapcar (lambda (x) `(print ,`,x)) ',args)
              ,(mapcar (lambda (x) (print x)) ',args)
              (print "expanding outer macro")))
       )
    ))

(test_d_macro this_macro (a b c d)
              (+ (a b))
              (+ 2 3)
              (* 5 7))

(this_macro)

(this_macro 1 2 3 4 5 6 7 8 9 10)
