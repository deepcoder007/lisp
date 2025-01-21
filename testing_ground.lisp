

;; alambda
(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))


(alambda arg1 arg2 arg3 arg4)

(alambda (((x y) (+x y))))

(alambda (((x y)
           (+ x y))))

(setq test_fn (labels ((self (x y)
                (+ x y)))
  #'self))

(funcall test_fn 1 2)

;; alet function
(defmacro alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

(setq test_alet (alet ((a 10) (b 20) (c 30))
      (setq d (+ a b c))
      (lambda (x)
        (+ x d))))

(funcall test_alet 1000)

;;; alet-fsm
(defmacro alet-fsm (&rest states)
  `(macrolet ((state (s)
                `(setq this #',s)))
     (labels (,@states) #',(caar states))))

(alet-fsm
    (going-up (n)
        (if (eq n 'invert)
            (state going-down)
            (incf acc n)))
    (going-down (n)
        (if (eq n 'invert)
            (state going-up)
            (decf acc n))))

(defun flatten (x)
  (labels ((rec (x acc)
                (cond ((null x) acc)
                      ((typep x 'sb-impl::comma) (rec (sb-impl::comma-expr x) acc))
                      ((atom x) (cons x acc))
                      (t (rec
                            (car x)
                            (rec (cdr x) acc))))))
    (rec x nil)))

(defun sample_fn (x y)
                (cond ((< x 10) 10)
                        ((> x 100) 100)
                        (t 0)))

(sample_fn 110 0)

(defun g!-symbol-p (s)
    (and (symbolp s)
            (> (length (symbol-name s)) 2)
            (string= (symbol-name s)
                    "G!"
                    :start1 0
                    :end1 2)))

(g!-symbol-p 'G!value-symbol)

(g!-symbol-p 'value-wo-symbol)

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
              (remove-if-not #'g!-symbol-p
                             (flatten body)))))
    `(print ,syms)
  )
)

(defmacro/g! sample_fn (x y)
                     (progn
                         (+ x y G!-test)
                         (+ x y G!-test-2)
                         (+ x y G!-test)
                         (+ x y G!-test-3)
                     ))
