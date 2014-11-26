(load "main.lisp")
(load "clause-form.lisp")

(setf t1 (list (make-predicate :sym #\P)
               (make-lvar :sym #\x)
               (list (make-predicate :sym #\g)
                     (make-lvar :sym #\x))
               (list (make-predicate :sym #\g)
                     (list (make-predicate :sym #\f)
                           (make-constant :sym #\a)))))

(setf t2 (list (make-predicate :sym #\P)
               (list (make-predicate :sym #\f)
                     (make-lvar :sym #\u)
                     )
               (make-lvar :sym #\v)
               (make-lvar :sym #\v) ))

(format t "final: ~A ~%" (unify t1 t2)) 

(setf x (list (make-land) 
              (list (make-land) 
                    (list (make-latom :sym #\P) 
                                      #\x 
                                      #\y)
                    (list (make-latom :sym #\Q) 
                          #\x))
              (list (make-latom :sym #\N) 
                    #\x)
              (list (make-lor) 
                    (list (make-latom :sym #\D) 
                          #\a)
                    (list (make-latom :sym #\V)
                          #\b))
              (list (make-latom :sym #\D)
                    #\x)))
(format t "~A ~%" (flatten x))
