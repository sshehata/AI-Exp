(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
(load "unifier.lisp")
(load "clause-form.lisp")

(defparameter t1 
  (list (make-predicate :sym #\P)
        (make-lvar :sym #\x)
        (list (make-predicate :sym #\g)
              (make-lvar :sym #\x))
        (list (make-predicate :sym #\g)
              (list (make-predicate :sym #\f)
                    (make-constant :sym #\a)))))

(defparameter t2 (list (make-predicate :sym #\P)
               (list (make-predicate :sym #\f)
                     (make-lvar :sym #\u))
               (make-lvar :sym #\v)
               (make-lvar :sym #\v)))

(format t "~%Unification Example 1:~%")
(Unify t1 t2)

(defparameter t3 (list (make-predicate :sym #\P )
               (make-constant :sym #\a)
               (make-lvar :sym #\y)
               (list (make-predicate :sym #\f)
                     (make-lvar :sym #\y))))

(defparameter t4 (list (make-predicate :sym #\P)
               (make-lvar :sym #\z)
               (make-lvar :sym #\z)
               (make-lvar :sym #\u)))

(format t "~%Unification Example 2:~%")
(Unify t3 t4)

(defparameter t5 (list (make-predicate :sym #\f)
               (make-lvar :sym #\x)
               (list (make-predicate :sym #\g)
                     (make-lvar :sym #\x))
               (make-lvar :sym #\x)))

(defparameter t6 (list (make-predicate :sym #\f) 
               (list (make-predicate :sym #\g)
                     (make-lvar :sym #\u)
                     )
               (list (make-predicate :sym #\g)
                     (list (make-predicate :sym #\g)
                           (make-lvar :sym #\z))) 
               (make-lvar :sym #\z)))

(format t "~%Unification Example 3:~%")
(Unify t5 t6)

(defparameter y (list (make-there-exists :sym #\x)
              (list (make-land)
                    (list (make-latom :sym #\P)
                          #\x)
                    (list (make-forall :sym #\x)
                          (list (make-limpl)
                                (list (make-latom :sym #\Q)
                                      #\x)
                                (list (make-lnot)
                                      (list (make-latom :sym #\P)
                                            #\x
                                            )))))))

(format t "~%clause-form 1: ~%")
(ClauseForm y)

(defparameter z (list (make-forall :sym #\x)
              (list (make-leq)
                    (list (make-latom :sym #\P)
                          #\x)
                    (list (make-land)
                          (list (make-latom :sym #\Q)
                                #\x)
                          (list (make-there-exists :sym #\y)
                                (list (make-land)
                                      (list (make-latom :sym #\Q)
                                            #\y)
                                      (list (make-latom :sym #\R)
                                            #\y
                                            #\x)))))))

(format t "~%clause-form 2: ~%")
(ClauseForm z)

