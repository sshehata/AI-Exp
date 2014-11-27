;; =========================================
;; ============= DATA TYPES ================
;; =========================================

(defstruct predicate
  sym) 

(defstruct constant
  sym)

(defstruct lvar 
  sym)

(defstruct (binding
             (:print-function
               (lambda (struct stream depth)
                 (declare (ignore depth))
                 (format stream "{~A / ~A}" 
                         (print-expr (binding-term struct))
                         (print-expr (binding-lvar struct)))))) 
  term lvar)

(defstruct fail)

;; =========================================
;; ============= TOP LEVEL FUNCTION ========
;; =========================================

(defun Unify (E1 E2)
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
  (let ((binding (bound-p x mu)))
    (if binding 
      (unify1 (binding-term binding) e mu)
      (progn
        (let ((term (ground mu e)))
          (if (occurs-p x term)
            (make-fail) 
            (let ((new-mu (list (make-binding :term term :lvar x ))))
              (append (update-mu new-mu mu) new-mu))))))))

;; =========================================
;; ========== HELPER FUNCTIONS =============
;; =========================================

(defun atomp (E1)
  (or (predicate-p E1)
      (constant-p E1)))

(defun bound-p (x mu)
  (find-if #'(lambda (e)
               (equalp (binding-lvar e) x))
           mu))

(defun ground (mu e)
  (if (consp e)
    (cons (ground mu (car e))
          (ground mu (cdr e)))
    (if (lvar-p e)
      (let ((binding (bound-p e mu)))
        (if binding
          (binding-term binding)
          e))
      e)))

(defun occurs-p (x term)
  (if (consp term)
    (or (occurs-p x (car term))
         (occurs-p x (cdr term)))
    (equalp x term)))

(defun update-mu (new-mu mu)
  (mapcar #'(lambda (binding)
              (make-binding :term (ground new-mu (binding-term binding))
                            :lvar (binding-lvar binding)))
          mu))


;; =========================================
;; ============= PRINTING ==================
;; =========================================

(defun print-expr (expr)
  (if (consp expr)
    (format nil "~A(~{~A~^, ~})" (predicate-sym (car expr))
            (mapcar #'print-expr (cdr expr)))
    (if (constant-p expr)
      (format nil "~C" (constant-sym expr))
      (format nil "~C" (lvar-sym expr)))))

