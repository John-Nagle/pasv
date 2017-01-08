(declare (macros t))

(defun progvn macro (arg)
  ; progvn is a combination of prog and progn
  ; (progvn (vars) E1 E2 ... En) creates variables vars, but
  ;    returns the value of En.  Note that no Ei may be a return.
  ;  
  `(prog ,(cadr arg) (return (progn ,@(cddr arg)))))
