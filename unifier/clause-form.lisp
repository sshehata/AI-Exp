;; =========================================
;; ============= DATA TYPES ================
;; =========================================

(defstruct land)
(defstruct lor)
(defstruct limpl)
(defstruct leq)
(defstruct lnot)
(defstruct latom
  sym)
(defstruct forall
  sym)
(defstruct there-exists
  sym)
(defstruct pair
  sym repl)


;; =========================================
;; ============= TOP LEVEL FUNCTION ========
;; =========================================

(defun ClauseForm (expr)
 (rename-clauses 
   (clauses 
     (flatten 
       (discard-forall 
         (skolemize 
           (standarise-apart 
             (push-not 
               (remove-impl 
                 (remove-eql expr))))))))))


;; =========================================
;; ========= CLAUSE FORM STEPS =============
;; =========================================

(defun remove-eql (expr)
  (if (not (listp expr))
    expr
    (let ((operator (car expr)))
      (if (leq-p operator)
        (let ((left-side (remove-eql (car (cdr expr))))
              (right-side (remove-eql (car (cdr (cdr expr))))))
          (list (make-land) 
                (list (make-limpl) left-side right-side)
                (list (make-limpl) right-side left-side)))
        (apply-on-operands operator (cdr expr) #'remove-eql)))))

(defun remove-impl (expr)
  (if (not (listp expr))
    expr
    (let ((operator (car expr)))
      (if (limpl-p operator)
        (let ((left-side (remove-impl (car (cdr expr))))
              (right-side (remove-impl (car (cdr (cdr expr))))))
          (list (make-lor) 
                (list (make-lnot) left-side)
                right-side))
        (apply-on-operands operator (cdr expr) #'remove-impl)))))

(defun push-not (expr)
  (let ((operator (car expr)))
    (if (latom-p operator)
      expr
      (if (lnot-p operator)
        (push-not-helper (car (cdr expr)))
        (apply-on-operands operator (cdr expr) #'push-not)))))

(defun standarise-apart (expr)
  (defparameter *USED_VARS* nil)
  (defparameter *UNUSED_VARS* '(#\z #\y #\x #\w #\v #\u #\t #\s #\r #\q #\p #\o #\n #\m #\l #\k #\j #\i
                                #\h #\g #\f #\e #\d #\c #\b #\a))
  (standarise-apart-helper expr))

(defun skolemize (expr)
  (defparameter *SKOLEM_VARS* '(#\Z #\Y #\X #\W #\V #\U #\T #\S #\R #\Q #\P #\O #\N #\M #\L #\K #\J #\I
                                #\H #\G #\F #\E #\D #\C #\B #\A))
  (skolemize-helper expr))

(defun discard-forall (expr)
  (let ((operator (car expr)))
    (if (latom-p operator)
      expr
      (if (forall-p operator)
       (car (cdr expr))
        (apply-on-operands operator (cdr expr) #'discard-forall)))))


(defun flatten (expr)
  (let ((operator (car expr)))
    (if (latom-p operator)
      expr
      (if (land-p operator)
        (append (list (make-land)) (mapcan #'flatten-and (mapcar #'flatten (cdr expr))))
        (if (lor-p operator)
         (flatten-or (mapcar #'flatten (cdr expr))))))))

(defun clauses (expr)
  (if (latom-p (car expr))
    expr
    (mapcar #'clauses (cdr expr))))

(defun rename-clauses (expr)
  (defparameter *UNUSED_VARS* '(#\z #\y #\x #\w #\v #\u #\t #\s #\r #\q #\p #\o #\n #\m #\l #\k #\j #\i
                                #\h #\g #\f #\e #\d #\c #\b #\a))
  (mapcar #'(lambda (x)
              (defparameter *PAIRS* nil)
              (rename-clauses-helper x))
          expr))

;; =========================================
;; ============ HELPER FUNCTIONS ===========
;; =========================================

(defun push-not-helper (expr)
  (let ((operator (car expr)))
    (if (land-p operator)
      (let ((new-expr (list (make-lor))))
        (do ((term (cdr expr) (cdr term)))
          ((null term ) (reverse new-expr))
          (setf new-expr (cons (push-not-helper (car term)) new-expr))))
      (if (lor-p operator)
        (let ((new-expr (list (make-land))))
          (do ((term (cdr expr) (cdr term)))
            ((null term) (reverse new-expr))
            (setf new-expr (cons (push-not-helper (car term)) new-expr))))
        (if (forall-p operator)
          (list (make-there-exists :sym (forall-sym operator)) (push-not-helper (car (cdr expr))))
          (if (there-exists-p operator)
            (list (make-forall :sym (there-exists-sym operator)) (push-not-helper (car (cdr expr))))
            (if (lnot-p operator)
              (car (cdr expr))
              (list (make-lnot) expr))))))))

(defun standarise-apart-helper (expr)
  (let ((operator (car expr)))
    (if (latom-p operator)
      expr
      (if (forall-p operator)
        (if (member (forall-sym operator) *USED_VARS*)
          (rename (cdr expr) (forall-sym operator))
          (setf *USED_VARS* (cons (forall-sym operator) *USED_VARS*)))
        (if (there-exists-p operator)
          (if (member (there-exists-p operator) *USED_VARS*)
            (rename (cdr expr) (there-exists-sym operator))
            (setf *USED_VARS* (cons (there-exists-sym operator) *USED_VARS*)))
          (apply-on-operands operator (cdr expr) #'standarise-apart-helper))))))


(defun rename (expr var)
  (let ((new-var nil))
    (do ((unused-vars *UNUSED_VARS* (cdr unused-vars)))
      ((not (member (car unused-vars) *USED_VARS*)) (setf new-var (car unused-vars)) 
                                                    (setf *UNUSED_VARS* unused-vars)
                                                    (setf *USED_VARS* (cons new-var *USED_VARS*)))) 
    (rename-helper expr new-var var)))

(defun rename-helper (expr new-var var)
  (let ((operator (car expr)))
    (if (latom-p operator)
      (let ((new-expr (list operator)))
        (do ((term (cdr expr) (cdr term)))
          ((null term) (reverse new-expr))
          (if (equalp var (car term))
            (setf new-expr (cons new-var new-expr))
            (setf new-expr (cons (car term) new-expr)))))    
      (let ((new-expr (list operator)))
        (do ((term (cdr expr) (cdr term)))
          ((null term) (reverse new-expr))
          (setf new-expr (cons (rename-helper (car term) new-var var) new-expr)))))))

(defun skolemize-helper (expr)
  (let ((operator (car expr)))
    (if (latom-p operator)
      expr
      (if (there-exists-p operator)
        (let ((skolem-var (car *SKOLEM_VARS*)))
          (setf *SKOLEM_VARS* (cdr *SKOLEM_VARS*)) 
          (rename-helper (car (cdr expr)) skolem-var (there-exists-sym operator)))
        (apply-on-operands operator (cdr expr) #'skolemize-helper)))))

(defun flatten-and (expr)
  (if (land-p (car expr))
    (cdr expr)
    (list expr)))

(defun flatten-or (expr)
  (flatten-or-helper (cons (append (list (make-lor)) (filter-atoms expr))
                           (filter-ops expr))))

(defun flatten-or-helper (expr)
  (if (null expr)
    nil
    (let ((left-side (car expr))
          (right-side (flatten-or-helper (cdr expr))))
      (combine left-side right-side))))

(defun combine (left-side right-side)
  (if (null right-side)
    left-side
    (let ((left-op (car left-side))
          (right-op (car right-side)))
      (if (and (lor-p left-op)
               (lor-p right-op))
        (append (list (lor-p))
                (cdr left-side)
                (cdr right-side))
        (if (lor-p left-op)
          (combine-and-or (cdr right-side) (cdr left-side))
          (if (lor-p right-op)
            (combine-and-or (cdr left-side) (cdr right-side))
            (combine-and-and (cdr left-side) (cdr right-side))))))))

(defun combine-and-or (and-list or-list)
  (append (list (make-land))
          (mapcar #'(lambda (x)
                      (append (list (make-lor))
                              (cons x or-list)))
                  and-list)))

(defun combine-and-and (and-list1 and-list2)
  (append (list (make-land))
          (mapcan #'(lambda (x)
                      (cdr x))
                  (mapcar #'(lambda (x)
                              (combine-and-or and-list1 (list x)))
                          and-list2)  )))

(defun filter-atoms (expr)
  (mapcan #'(lambda (x) 
              (when (latom-p (car x)) (list x)))
          expr))

(defun filter-ops (expr)
  (mapcan #'(lambda (x)
              (when (not (latom-p (car x))) (list x)))
          expr))

(defun apply-on-operands (operator expr fun)
  (let ((new-expr (list operator)))
    (do ((term expr (cdr term)))
      ((null term) (reverse new-expr)) (setf new-expr (cons (funcall fun (car term)) new-expr)))))

(defun rename-clauses-helper (expr)
  (if (null expr)
    nil
    (if (listp expr)
      (cons (rename-clauses-helper (car expr))
            (rename-clauses-helper (cdr expr)))
      (if (latom-p expr)
        expr
        (let ((repl (replacedp expr *PAIRS*)))
          (if repl
            repl
            (progn (setf repl (car *UNUSED_VARS*))
                   (setf *UNUSED_VARS* (cdr *UNUSED_VARS*))
                   (setf *PAIRS* (cons (make-pair :sym expr :repl repl) *PAIRS*))
                   repl)))))))

(defun replacedp (expr pairs)
  (if (null pairs)
    nil
    (if (equalp (pair-sym (car pairs)) expr)
      (pair-repl (car pairs))
      (replacedp expr (cdr pairs)))))
