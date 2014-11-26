(defstruct predicate
  sym) 

(defstruct constant
  sym)

(defstruct lvar 
  sym)

(defstruct binding 
  term lvar)

(defstruct fail)

(defun unify (E1 E2)
  (unify1 E1 E2 nil))

(defun unify1 (E1 E2 mu)
  (if (and (null E1)
           (null E2))
    (return-from unify1 mu))
  (if (or (null E1)
          (null E2))
    (return-from unify1 (make-fail)))
  (if (fail-p mu)
    (return-from unify1 mu))
  (if (equalp E1 E2)
    (return-from unify1 mu))
  (if (lvar-p E1)
    (return-from unify1 (unify-var E1 E2 mu)))
  (if (lvar-p E2)
    (return-from unify1 (unify-var E2 E1 mu)))
  (if (or (atomp E1)
          (atomp E2))
    (return-from unify1 (make-fail)))
  (if (not (equalp (length E1)
                   (length E2)))
    (return-from unify1 (make-fail)))
  (unify1 (cdr E1) (cdr E2) (unify1 (car E1) (car E2) mu)))

(defun unify-var (x e mu)
  (let ((term (bound-p x mu)))
    (if term
      (return-from unify-var (unify1 term e mu)))
    (if (listp e)
      (setf term (ground mu e))
      (setf term e))
    (if (listp term)
      (if (occurs-p x term)
        (return-from unify-var (make-fail))))
    (cons (make-binding :term term :lvar x) mu)))

(defun atomp (E1)
  (or (predicate-p E1)
      (constant-p E1)))

(defun bound-p (x mu)
  (if (null mu)
    nil
    (if (equalp (binding-lvar (car mu)) x)
      (binding-term (car mu))
      (bound-p x (cdr mu)))))

(defun ground (mu e)
  (if (null e)
    nil
    (if (not (atomp (car e)))
      (let ((term (bound-p (car e) mu)))
        (if term
          (cons term (ground mu (cdr e)))
          (cons (car e) (ground mu (cdr e)))))
      (cons (car e) (ground mu (cdr e))))))

(defun occurs-p (x term)
  (if (null term)
    nil
    (if (equalp (car term) x)
      T
      (occurs-p x (cdr term)))))
