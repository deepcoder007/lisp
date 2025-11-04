

(defmacro hello-world ()
  (print "hello world fn let over lambda  v1"))

(defvar sample_code '(+ x x))
(defvar sample_code_2 '(+ y y))

(let ((x 10100))
  (eval sample_code))

(let ((x 101))
  (eval sample_code))

(let ((y 10100))
  (eval sample_code_2))


(defmacro expand-str (name args &rest body)
  `(defun ,name ,args
     (progn ,@body
            (print "hello-world"))))

(defmacro expand-str (name args &rest body)
  (let ((x 1))
     (print (symbol-name name))
     (mapcar (lambda (x) (print (symbol-name x))) args)
     (print "expand-str-exec")
     (mapcar (lambda (x)
               (if (symbolp x) (print (symbol-name x)) (print "not a symbol")))
             body)
    ))

(expand-str f_name (a b c) (print "this is body") (print "this is second") (print "third"))

(expand-str f_name (a b c)
            'x
            'y
            (print "this is body")
            'z)

(f_name 1 2 3)

(defmacro expand_x (x)
  `(eval ,x))

(expand_x xx)

(let ((xx 10101))
  (expand_x xx))

(defun group (source n)
  (if (zerop n) (error "Zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                              (subseq source 0 n)
                              acc))
                   (nreverse
                    (cons source acc))))))
    (if source (rec source nil) nil)))


(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x)
                           (rec (cdr x) acc))))))
    (rec x nil)))

(defmacro test-var-name (x)
  (print (symbol-name x)))


(defvar x-val)

(setq x-val '(1101 102))

`(list 1 2 ,@x-val)


(defvar to-splice '(b c d))

`(a ,.to-splice e)


(let* ((x 10))
  (progn
    (print "BEGIN")
    (print x)
    (print "END")))


(loop for x from 1 to 10 collect x)

(loop for x from 1 to 10
      do (print x))


(defparameter some-fn (let ((x 101))
                        (lambda () (print x))))

(funcall some-fn)

;; G-bang symbol


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

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-if-not #'g!-symbol-p
                             (flatten body))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(subseq
                               (symbol-name s)
                               2))))
              syms)
         ,@body))))



(defmacro/g! nif (expr pos zero neg)
  `(let ((,g!result ,expr))
     (cond ((plusp ,g!result) ,pos)
           ((zerop ,g!result) ,zero)
           (t ,neg))))



(defmacro/g! nif (expr pos zero neg)
  `(let ((,g!result ,expr))
     (cond ((plusp ,g!result) ,pos)
           ((zerop ,g!result) ,zero)
           (t ,neg))))
