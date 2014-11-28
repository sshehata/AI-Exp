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

(defun ClauseForm (expr &optional visual)
  (visualise "Input" expr visual #'print-fol)
  (mapc #'(lambda (x)
            (setf expr (funcall x expr))
            (visualise x expr visual #'print-fol))
        '(remove-eql
           remove-impl
           push-not
           standarise-apart
           skolemize
           discard-forall
           flatten))
  (setf expr (clauses expr))
  (visualise 'clauses expr visual #'print-clause-form)
  (setf expr (rename-clauses expr))
  (format t "STANDARISE-APART-CLAUSES:~%~A ~%" 
          (print-clause-form expr)))

(defun visualise (name expr visual fun)
  (if visual
    (progn 
    (format t "~A:~%~A ~%" name (funcall fun expr)) 
      (read-char))))


;; =========================================
;; ========= CLAUSE FORM STEPS =============
;; =========================================

(defun remove-eql (expr)
  (if (not (listp expr))
    expr
    (let ((operator (car expr)))
      (if (leq-p operator)
        (let ((left-side (remove-eql (second expr)))
              (right-side (remove-eql (third expr))))
          (list (make-land) 
                (list (make-limpl) left-side right-side)
                (list (make-limpl) right-side left-side)))
        (cons operator (mapcar #'remove-eql (cdr expr)))))))

(defun remove-impl (expr)
  (if (not (listp expr))
    expr
    (let ((operator (car expr)))
      (if (limpl-p operator)
        (let ((left-side (remove-impl (second expr)))
              (right-side (remove-impl (third expr))))
          (list (make-lor) 
                (list (make-lnot) left-side)
                right-side))
        (cons operator (mapcar #'remove-impl (cdr expr)))))))

(defun push-not (expr)
  (typecase (car expr)
    (lnot (push-not-helper (second expr)))
    (latom expr)
    (otherwise (cons (car expr)
                     (mapcar #'push-not 
                             (cdr expr))))))

(defun standarise-apart (expr)
  (defparameter *USED_VARS* nil)
  (defparameter *UNUSED_VARS* '(#\z #\y #\x #\w #\v #\u #\t #\s #\r #\q #\p #\o
                                #\n #\m #\l #\k #\j #\i #\h #\g #\f #\e #\d #\c
                                #\b #\a))

  (standarise-apart-helper expr))

(defun skolemize (expr)
  (defparameter *SKOLEM_VARS* '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L
                                #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X
                                #\Y #\Z))
  (skolemize-helper expr))

(defun discard-forall (expr)
  (typecase (car expr)
   (forall (discard-forall (second expr)))
   (latom expr)
   (otherwise (cons (car expr)
                    (mapcar #'discard-forall (cdr expr))))))


(defun flatten (expr)
  (typecase (car expr)
    (land (cons (make-land)
                (mapcan #'flatten-and 
                        (mapcar #'flatten (cdr expr)))))
    (lor (flatten-or (mapcar #'flatten
                             (cdr expr))))
    (otherwise expr)))

(defun clauses (expr)
  (if (or (latom-p (car expr))
          (lnot-p (car expr)))
    expr
    (mapcar #'clauses (cdr expr))))

(defun rename-clauses (expr) 
  (defparameter *UNUSED_VARS* '(#\z #\y #\x #\w #\v #\u #\t #\s #\r #\q #\p #\o
                                #\n #\m #\l #\k #\j #\i #\h #\g #\f #\e #\d #\c
                                #\b #\a))

  (defparameter *SKOLEM_VARS* '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L
                                #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X
                                #\Y #\Z))
  (mapcar #'(lambda (x)
              (defparameter *PAIRS* nil)
              (rename-clauses-helper x))
          expr))

;; =========================================
;; ============ HELPER FUNCTIONS ===========
;; =========================================

(defun push-not-helper (expr)
  (typecase (car expr)
    (land (cons (make-lor)
                (mapcar #'push-not-helper 
                        (cdr expr)))) 
    (lor (cons (make-land)
               (mapcar #'push-not-helper 
                       (cdr expr))) )
    (forall (list (make-there-exists :sym (forall-sym (car expr))) 
                  (push-not-helper (second expr))))
    (there-exists (list (make-forall :sym (there-exists-sym (car expr))) 
                        (push-not-helper (second expr))))
    (lnot (second expr))
    (otherwise (list (make-lnot)
                     expr))))

(defun standarise-apart-helper (expr)
  (typecase (car expr)
    (latom expr)

    (forall (let ((repl (find-repl (forall-sym (car expr)))))
              (list (make-forall :sym repl)
                    (rename (standarise-apart-helper (second expr))
                            repl
                            (forall-sym (car expr))))))
    
    (there-exists (let ((repl (find-repl (there-exists-sym (car expr)))))
                    (list (make-there-exists :sym repl)
                          (rename (standarise-apart-helper (second expr))
                                  repl
                                  (there-exists-sym (car expr))))) )
    (otherwise (cons (car expr)
                     (mapcar #'standarise-apart-helper (cdr expr))))))

(defun find-repl (var)
  (if (member var *USED_VARS*)
    (find-if #'(lambda (x)
                 (not (member x *USED_VARS*))) *UNUSED_VARS*)  
    var))

(defun rename (expr new-var var)
  (if (member var *USED_VARS*)
    (progn (setf *USED_VARS* (cons new-var *USED_VARS*))
           (rename-helper expr new-var var))
    (progn (setf *USED_VARS* (cons var *USED_VARS*))
           expr)))

(defun rename-helper (expr new-var var)
    (if (latom-p (car expr))
      (append (list (car expr))
              (mapcar #'(lambda (x)
                          (if (equalp x var)
                            new-var
                            x))
                      (cdr expr)))
      (append (list (car expr))
              (mapcar #'(lambda (x)
                          (rename-helper x new-var var))
                      (cdr expr)))))

(defun skolemize-helper (expr)
  (typecase (car expr)
    (latom expr)
    (there-exists (let ((skolem-var (car *SKOLEM_VARS*)))
                    (setf *SKOLEM_VARS* (cdr *SKOLEM_VARS*))
                    (rename-helper (second expr)
                                   skolem-var
                                   (there-exists-sym (car expr)))))
    (otherwise (cons (car expr)
                     (mapcar #'skolemize-helper
                             (cdr expr))))))

(defun flatten-and (expr)
  (if (land-p (car expr))
    (cdr expr)
    (list expr)))

(defun flatten-or (expr)
  (flatten-or-helper (cons (cons (make-lor) 
                                 (filter-atoms expr))
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
        (append (list (make-lor))
                (cdr left-side)
                (cdr right-side))
        (if (lor-p left-op)
          (combine-and-or (cdr right-side) (cdr left-side))
          (if (lor-p right-op)
            (combine-and-or (cdr left-side) (cdr right-side))
            (combine-and-and (cdr left-side) (cdr right-side))))))))

(defun combine-and-or (and-list or-list)
  (cons (make-land)
          (mapcar #'(lambda (x)
                      (append (list (make-lor))
                              (cons x or-list)))
                  and-list)))

(defun combine-and-and (and-list1 and-list2)
  (cons (make-land)
        (mapcan #'(lambda (x)
                    (cdr x))
                (mapcar #'(lambda (x)
                            (combine-and-or and-list1 (list x)))
                        and-list2))))

(defun filter-atoms (expr)
  (mapcan #'(lambda (x) 
              (when (or (latom-p (car x))
                        (lnot-p (car x))) (list x)))
          expr))

(defun filter-ops (expr)
  (mapcan #'(lambda (x)
              (when (not (or (latom-p (car x))
                             (lnot-p (car x)))) (list x)))
          expr))

(defun rename-clauses-helper (expr)
  (typecase expr
    (cons (cons (rename-clauses-helper (car expr))
                (rename-clauses-helper (cdr expr))))
    (list nil)
    (latom expr)
    (lnot expr)
    (otherwise  (let ((repl (replacedp expr *PAIRS*)))
                  (if repl
                    (pair-repl repl)
                    (if (upper-case-p expr)
                      (progn (setf repl (car *SKOLEM_VARS*))
                             (setf *SKOLEM_VARS* (cdr *SKOLEM_VARS*))
                             (setf *PAIRS* (cons (make-pair :sym expr :repl repl) 
                                                 *PAIRS*))
                             repl)
                      (progn (setf repl (car *UNUSED_VARS*))
                             (setf *UNUSED_VARS* (cdr *UNUSED_VARS*))
                             (setf *PAIRS* (cons (make-pair :sym expr :repl repl) 
                                                 *PAIRS*))
                             repl)))))))

(defun replacedp (expr pairs)
  (find-if #'(lambda (x)
               (equalp (pair-sym x) expr))
           pairs))

;; =========================================
;; ============= PRINTING ==================
;; =========================================

(defun print-fol (expr)
  (if (consp expr)
    (let ((operands (mapcar #'print-fol (cdr expr))))
      (typecase (car expr)
        (land (format nil "(~{~A~^ & ~})" operands))
        (lor (format nil "(~{~A~^ v ~})" operands))
        (limpl (format nil "(~{~A~^ impl ~})" operands))
        (leq (format nil "(~{~A~^ eq ~})" operands))
        (forall (format nil "forall ~A [ ~{~A~} ]"
                        (forall-sym (car expr))
                        operands))
        (there-exists (format nil "there-exists ~A [ ~{~A~} ]" 
                              (there-exists-sym (car expr))
                              operands))
        (latom (format nil "~A(~{~A~^,~})"
                       (latom-sym (car expr))
                       operands))
        (lnot (if (lnot-p (car expr)) 
                (format nil "not ~{~A~}"
                        operands)))))
    (format nil "~C" expr)))

(defun print-clause-form (expr)
  (format nil "{ ~{{ ~A }~^, ~}  }"
          (mapcar #'print-clause expr)))

(defun print-clause (expr)
  (if (listp (car expr))
    (format nil "~{~A~^, ~}"
            (mapcar #'print-atom expr)) 
    (print-atom expr)))

(defun print-atom (expr)
  (if (consp expr)
    (if (latom-p (car expr))
      (format nil "~A(~{~A~^, ~})"
              (latom-sym (car expr))
              (mapcar #'print-atom (cdr expr)))
      (format nil "not ~A" (print-atom (second expr))))
    (format nil "~C" expr)))
