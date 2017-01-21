;
;	Eform  --  enode form to list form conversion
;
;					Version 1.2 of 12/17/82

;;;(declare
;;;  (load 'need.o) (load 'defmac.o) (load 'hunkshell.o) (load 'enode.o)
;;;  (load 'debug.o) (load 'princ.o) (load 'map.o))
  
;;; (needs-macros)

(declarespecial
   eformlist
   enodelist
   falsenode
   quotednodelist     ; a list of dotted pairs of the form (f . n), where f
		      ; is an enode representing a function, and n is an 
		      ; integer.  The pair indicates that the n'th argument of
		      ; f is implicitly quoted.  Only one postition of any
		      ; function can be implicitly quoted.
   ruledepth
   truenode
   zfunctionnodes
   zgenode
   zgtnode
   zmultnode
   )
;;;============================================================================
;;; eform

(defun eform (node) ((lambda (eformlist) (cdr (eform2 (eroot node)))) nil)) 

(defun eform2 (root) 
       (prog (pair temp) 
             (and (eq root (eroot truenode)) (return '(1 . true)))
             (and (eq root (eroot falsenode)) (return '(1 . false)))
             (and (setq pair (assq root eformlist)) (return (cdr pair)))
             (setq pair (cons root (cons 200000 'printerror)))
             (setq eformlist (cons pair eformlist))
             (setq temp (eform3 root 200000 'printerror))
             (cond ((> (car temp) 199999)
		    (setq eformlist (delq pair eformlist)))
                   (t (rplacd pair temp)))
             (return temp))) 

(defun eform3 (finish oldsize oldform) 
       (prog (form x) 
             (setq x (eqclass finish))
        a    (setq form (eform4 x))
             (cond ((numberp (cdr form)) (return form))
                   ((> oldsize (car form)) (setq oldsize (car form)) 
                                           (setq oldform (cdr form))))
             (and (eq x finish) (return (cons oldsize oldform)))
             (setq x (eqclass x))
             (go a))) 

(defun eform4 (node) 
       (prog (form) 
             (setq form (esuccessors node))
             (return (cond ((econgruent node) '(200000 . printerror))
                           ((eprinttag node) '(200000 . printerror))
                           ((atom form) (cons 1 form))
                           ((eq (eroot (car form)) (eroot zgenode))
                            (ppzge (cadr form) (caddr form) nil))
                           ((eq (eroot (car form)) (eroot zgtnode))
                            (ppzge (cadr form) (caddr form) t))
                           ((eq (eroot (car form)) (eroot zmultnode))
                            (ppzmult (cadr form) (caddr form)))
                           ((and (memq (car form) zfunctionnodes) 
				 (zterm node))
					(ppzterm node))
                           ((assq (car form) quotednodelist) 
                            (eform7 (eform5 form 0 nil 
                                            (cdr (assq (car form) 
						       quotednodelist)))))
                           (t (eform7 (eform5 form 0 nil -1))))))) 

(defun eform5 (l size form n)
       (prog (pair) 
             (or l (return (cons size form)))
             (setq pair (cond ((= n 0) (eform4 (car l)))
                              (t (eform2 (eroot (car l))))))
             (and (> (car pair) 199999) (return pair))
             (return (eform5 (cdr l) (+ size (car pair)) 
                             (cons (cdr pair) form) (1- n))))) 

(defun eform7 (pair) 
       (cond ((> (car pair) 199999) pair) (t (rplacd pair (nreverse (cdr pair)))))) 
