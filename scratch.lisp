

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
               (print "this is macro")
              `(+ ,@fargs)))

(this_macro 1 2 (3 4 5) (6 7 (8 9 10)))


(defmacro test_c_macro (name args &rest body)
  (let ((syms (flatten body)))
    `(print ,syms)))


(test_c_macro this_macro (a b)
              (+ a b))

(this_macro 1 2)
