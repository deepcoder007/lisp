

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x)
                           (rec (cdr x) acc))))))
    (rec x nil)))


(flatten '(1 2 3))



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

'(defmacro proc_)

(defmacro test_c_macro (name args &rest body)
  (let ((syms (flatten body)))
     (print "--------- expand test_c_macro ---------")
     (print syms)
     (print name)
     (print (symbol-name name))
    `(defmacro ,name (&rest arg_list)
         (progn (print arg_list)
                (print (length arg_list))
                '(print (mapcar (lambda (x) (symbol-name x))
                                (cdr args)))
                '(mapcar (lambda (x) (print (symbol-name x)))
                                (cdr args))
                ,@(mapcar (lambda (x)
                            `(progn
                               (print ,(symbol-name x))
                               (print ',x)))
                                args)
                (print "before value")
                (mapcar (lambda (x)
                            (progn
                               (print x)))
                                arg_list)
                (print "after value")))
))

(test_c_macro this_macro (a b c d)
              (+ (a b)))

(this_macro 1 2 3 4 5 6 7 8 9 10)

; (1 2 3 4 5 6 7 8 9 10)
; 10
; "A"
; A
; "B"
; B
; "C"
; C
; "D"
; D
; "value"  => "value"
