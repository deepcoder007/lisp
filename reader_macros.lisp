


(defun hello-world ()
  (print "hello world fn"))

(mapcar (lambda (n) (+ n 1)) '(1 2 3 4 5 6))

(mapcar #l(+ %n 1) '(1 2 3 4 5 6))


(set-macro-character #p
            #`(lambda (stream char)
                (list 'quote (read stream t nil t))))

;; some testing on macros expressions in lisp

(defmacro gen_list_macro (N arg1)
    `((print ,arg1)
        (print ,N)
        (print ,arg1)
        (print ,N)))


(defun gen_list_fn (N arg1)
  (loop for x from 1 to N collect
        (print arg1)))


(defmacro test_macro (name arg1 arg2)
  (loop for x from 1 to arg1 collect
  `(print ,name)))

(macroexpand-1 '(test_macro 1 10 3))
