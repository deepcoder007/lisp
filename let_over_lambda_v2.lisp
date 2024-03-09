
;; clean slate

(defvar test_dcap)

(let ((temp-special '120))
  (setq test_dcap (lambda () temp-special)))


(defvar temp-special)
(defvar temp-special-2)

(setq temp-special '123)

(let ((temp-special-2 temp-special))
  (setq test_dcap (lambda () temp-special-2)))


(setq temp-special '129)

(funcall test_dcap)



(defvar temp-val)



(defvar test-dcap (lambda () temp-val))


;;;;;;;;;;;;;;;;;;

(defvar x)

(setq x 10)

(let ((x 1))
  (progn (print x)
         (let ((x 2))
           (print x))
         (print x)
         )
  )


(let ((s 'hello))
  `(,s world))
