(load "main.lisp")

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
