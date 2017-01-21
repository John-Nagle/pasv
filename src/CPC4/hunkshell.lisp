"@(#)hunkshell.l	2.2"

;;;============================================================================
;;; hunkshell 
;;;
;;; hunkshell simulates record structures using hunks.
;;; 
;;; For example a node of a sparse matrix of rational numbers might
;;; contain fields for a numerator, a denominator, fields for its row
;;; and column, and links to the nodes above and to the left of it.
;;; We could use the call
;;; 
;;; (hunkshell matrix-node num den row col up left)
;;; 
;;; to "define a new type" matrix-node with six components named num, den,
;;; row, col, up, and left.   (Of course no new data type is actually added
;;; to lisp.)  The effect of the call to hunkshell is to define thirteen macros,
;;; one for creating matrix-nodes, and two for accessing and updating each of
;;; the six fields.    The creation macro is called alloc-matrix-node; a call to
;;; it returns a hunk that can hold six values.
;;; In general, the name of the allocation macro is always "alloc-" 
;;; concatenated with the first argument to hunkshell.   For each field,
;;; its corresponding access macro is just the field name; for instance,
;;; (num x) accesses the num field of x.   For each field, the update macro
;;; is x concatenated with the field name; thus (xnum x y) would update
;;; the num field of x to y. 

;;;;(declare (macros t))

;;; OBSOLETE - will not work in Common LISP.  Macros syntax has changed.

;;;(defun hunkshell macro (l) 
;;;       (progn (setq l (cdr l))
;;;              (cons 'progn 
;;;	            (cons ''compile
;;;                          (cons (define-alloc (car l) (length (cdr l)))
;;;                                (append (define-accesses (cdr l) 0)
;;;                                        (define-updates (cdr l) 0)))))))


;;;(defun define-alloc (name shelllen) 
;;;       (list 'defun (concat 'alloc- name) 'macro  '(app)
;;;             (list 'quote (list 'makhunk shelllen))))

;;;(defun define-accesses (l n) 
;;;      (and l (cons (define-access (car l) n)
;;;                    (define-accesses (cdr l) (1+ n)))))

;;;(defun define-updates (l n) 
;;;       (and l (cons (define-update (car l) n)
;;;                    (define-updates (cdr l) (1+ n)))))

;;;(defun define-access (name num) 
;;;       (list 'defun  name 'macro '(app)
;;;             (list 'cons (list 'quote 'cxr) (list 'cons num '(cdr app)))))

;;;(defun define-update (name num) 
;;;      (list 'defun (concat 'x name) 'macro '(app)
;;;            (list 'list ''rplacx  num  '(cadr app) '(caddr app))))
                                        
;   New Common LISP version, using structures instead of hunks.                                        
(defmacro hunkshell (hunkname &rest fields)
    (append 
        (list 'progn
            `(defstruct 
                (,hunkname (:conc-name nil) (:constructor ,(concat 'alloc- hunkname))) ; gen structs, allocator, and access fns
                    ,@fields))
        (define-updates hunkname fields))) ; gen update fns
                 
(defun define-updates (hunkname fields)
    (map 'list #'(lambda (field) (define-update hunkname field)) fields))
            
;;;;(defun define-update (hunkname field)            ; gen one update function
;;;;    `(defmacro ,(concat 'x field) (recvar newval) (quote (setf (,field recvar) newval))))
                                        

(defun define-update (hunkname field)            ; gen one update function
    `(defun ,(concat 'x field) (recvar newval) 
        (setf (,field recvar) newval)))
        
;;; We want the above to generate  (defmacro xwest (r v) `(setf (west ,r) ,v))
;;; Above ought to be a macro, but right now it's a function. Works.


