;;; UNUSED FILE 
;;;	Rule Insertion
;;;					Version 1.3 of 10/14/82
;;;
(declare (special freevariables ruletag instantiatetag))
;
;	addrule  --  add a new rule
;
(defun addrule (pattern formula variables)
       (prog (freevariables f)
             (setq freevariables variables)
             (setq f (prepattern pattern))
             (rplacd (last f) (ncons (list 'applyrule nil formula)))
             (ifexists nil nil nil f)))

(defunobj applyrule (node pmatchlist lab pattern)
          ((lambda (ruletag instantiatetag)
                  (setq pattern (instantiate pattern))
                  (cond (instantiatetag)
                        (t (propagate pattern))))
          t nil))

