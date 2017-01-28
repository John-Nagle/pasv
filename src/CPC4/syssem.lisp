;   UNUSED FILE
"@(#)syssem.l	2.2"

;;; Useful primitives

(declare (special terms freevariables predicates functions rulelist
                  splitrule* functionnames predicatenames constantnames
                  ifexists ifthen split merge* perror constants psymbols
                  newrules plhs invariables boolsymeq promptindex)

	  (load 'need.o)
	  (load 'princ.o)
	  (load 'map.o))

(needs-macros)

(progn
 (array answers t 50)
 (setq promptindex 0)
 (setq perror nil)
 (setq newrules nil)
 (setq rulelist nil)
 (setq freevariables nil)
 (setq constants constantnames)
 (setq predicates predicatenames)
 (setq functions functionnames))

(defun perrormess1 (mess) (setq perror t) (princ-start-terpri mess) (perrorline))

(defun perrormess2 (mess1 mess2) 
       (setq perror t) (princ-start mess1) (princ-terpri mess2) (perrorline))

(defun pprompt () 
       (cond ((> promptindex 50)
              (princ-start-terpri '"Too many line numbers -- restarting at 1")
              (setq promptindex 1))
             (t (setq promptindex (1+ promptindex))))
       (prompt))

(defun prompt() (terpri) (princ '"{") (princ promptindex) (princ '"} "))

(defuna pcommand (answer)
        (setq perror nil)
        (store (answers promptindex) answer)
        t)

(defuna pcons (a b) (cons a b))

(defuna pnoop (l) l)

(defuna pnil (l) nil)

(defuna punary (op x) (list (cond ((get op 'psynonym)) (t op)) x))

(defuna pbinary (x op y) (list (cond ((get op 'psynonym)) (t op)) x y))

(defuna pbracketerm (nil l nil) l)

(defuna pstartlist (l) (list l))

(defuna paddlist (x nil l) (cons x l))

(defuna pfnterm (f nil l nil) (cons f l))
;;; Expressions and formulas

(defuna pformula (formula) 
        (cond ((numberp formula) (pform formula) formula)
              ((and (atom formula) (get formula 'provecommand))
               (funcall (get formula 'provecommand))
               '**noop**)
              (t (setq formula (simp formula))
                 (cond (perror '**error**)
                       (t (pform formula) formula)))))

(defuna pcond (nil if nil then nil else) (list 'cond if then else))

;;; Declarations and error messages for rules

(defun perrormessrule (lab mess1 mess2)
       (setq perror t) 
       (cond (lab (princ-start '"Error in rule ") (princ lab) (princ '": "))
             (t (princ-start '"Error:  ")))
       (princ mess1) 
       (princ-terpri mess2))

(defuna pdeclare (l) (setq psymbols (list constants freevariables functions predicates)))

(defuna pdeclarefree (nil l) 
        (mapcone (and (or (memq x predicates) 
                          (memq x terms)
                          (memq x functions)
                          (memq x constants))
                      (perrormess2 '"Illegal free variable: " x))
                 l)
        (setq freevariables (append freevariables l)))

(defuna pdeclarepredicates (nil l) 
        (mapcone (and (or (memq x freevariables) 
                          (memq x constants)
                          (memq x functions))
                      (perrormess2 '"Illegal predicate: " x))
                 l)
        (setq predicates (append l predicates)))

(defuna pdeclarefunctions (nil l) 
        (mapcone (and (or (memq x freevariables) 
                          (memq x constants) 
                          (memq x predicates))
                      (perrormess2 '"Illegal function: " x))
                 l)
        (setq functions (append l functions)))

(defuna pdeclareconstants (nil l) 
        (mapcone (and (or (memq x freevariables) 
                          (memq x functions) 
                          (memq x predicates))
                      (perrormess2 '"Illegal constant: " x))
                 l)
        (setq constants (append l constants)))

(defun ispredicate (f)
       (cond ((memq f '(true false)))
             ((atom f) (memq f predicates))
             ((memq (car f) predicates))))
       
(defun isfunction (f)
       (cond ((atom f) (memq f functions))
             ((memq (car f) functions))))
       
(defun isequality (f) (and (not (atom f)) (eq (car f) boolsymeq)))

(defuna prule (lhs nil rhs) 
        (setq plhs lhs)
        (setq newrules 
              (cons (cons psymbols (cons nil (list 'ifexists lhs (pmerge (prhs rhs))))) newrules)))

(defuna plabelrule (lab nil lhs nil rhs) 
        (setq plhs lhs)
        (or lab (perrormess1 '"NIL illegal label."))
        (setq newrules 
              (cons (cons psymbols (cons lab (list 'ifexists lhs (pmerge (prhs rhs))))) newrules)))

(defun prhs (f)
       (cond ((atom f) f)
             ((eq (car f) 'cond) 
              (pifthenelse (prhs (cadr f)) (prhs (caddr f)) (prhs (cadddr f))))
             ((eq (car f) 'ifthen)
              (pifthen1 (prhs (cadr f)) (prhs (caddr f))))
             (t (mapcar 'prhs f))))

(defuna pifthen (nil if nil then) (list 'ifthen if then))

(defun pifthen1 (l then)
       (cond ((isequality l) 
              (list 'ifexists (cadr l) (list 'ifthen (caddr l) (pmerge then))))
             ((ispredicate l) (list 'ifexists l (list 'ifthen 'true (pmerge then))))
             ((isfunction l) (list 'ifexists l (pmerge then)))
             ((atom l) (perrormess2 '"Undeclared if clause: " l) nil)
             ((eq (car l) 'and) (pifthen2 (cadr l) (caddr l) then))
             (t (perrormess2 '"Undeclared if clause: " l) nil)))
             
(defun pifthen2 (and1 and2 then)
       (cond ((isequality and1) 
              (list 'ifexists (cadr and1) (list 'ifthen (caddr and1) 
                                                (pifthen1 and2 then))))
             ((ispredicate and1)
              (list 'ifexists and1 (list 'ifthen 'true (pifthen1 and2 then))))
             (t (perrormess2 '"Non predicate: " and1) nil)))

(defun pifthenelse (if then else)
       (cond ((isequality if)
              (list 'ifexists (cadr if) 
                    (list 'split (list 'ifthen (caddr if) (pmerge then))
                          (list 'ifexists if (list 'ifthen 'false (pmerge else))))))
             ((ispredicate if) 
              (list 'ifexists if (list 'split (list 'ifthen 'true (pmerge then))
                                       (list 'ifthen 'false (pmerge else)))))
             ((isfunction if) (perrormess2 '"Illegal function: " if) nil)
             ((atom if) (perrormess2 '"Undeclared if clause: " if) nil)
             ((eq (car if) 'and) (perrormess1 '"Illegal conjunction ") nil)))

(defun pmerge (x)
       (cond ((atom x) (list 'merge* plhs x))
             ((memq (car x) '(ifthen split ifexists)) x)
             (t (list 'merge* plhs x))))

(defun addrules () (mapcone (addrule (cadr x) (cddr x)) newrules) perror)

(defun addrule (lab rule)
       (setq rule (makerule lab (car rule) rule nil))
       (or perror (ifexists nil nil lab (caddr rule))))

(defun makerule (lab f rule invariables)
       (cond ((eq f 'ifexists) (makeif 'ifexists lab (cadr rule) (caddr rule)))
             ((eq f 'ifthen) (makeif 'ifthen lab (cadr rule) (caddr rule)))
             ((eq f 'split) (makesplit lab (cadr rule) (caddr rule)))
             ((eq f 'merge*) (makemerge lab (cadr rule) (caddr rule)))
             ((break makerule))))

(defun makeif (type lab if then)
       (and (memq if freevariables) 
            (not (memq if invariables))
            (perrormessrule lab '"Uninstantiated free variable: " if))
       (setq if (prepattern if))
       (rplacd (last if) (ncons (makerule lab (car then) then invariables)))
       (list (eval type) lab if))
       
(defun makemerge (lab lhs rhs)
       (cond ((not (instantiated lhs))
              (perrormessrule lab '"Uninstantiated expression: " lhs))
             ((not (instantiated rhs))
              (perrormessrule lab '"Uninstantiated expression: " rhs))
             (t (list merge* lab (cons lhs rhs)))))

(defun makesplit (lab split1 split2)
       (list split lab (cons (makerule lab (car split1) split1 invariables)
                             (makerule lab (car split2) split2 invariables))))

(defun instantiated (f)
       (cond ((not (atom f)) (and (instantiated (car f)) (instantiated (cdr f))))
             ((numberp f))
             ((memq f functions))
             ((memq f predicates))
             ((memq f constants))
             ((memq f invariables))
             (t (princ-start '"Uninstantiated: ") (princ-terpri f) (setq perror t) nil)))

(defun prepattern (f) 
       (cond ((isenode f) (ncons f))
             ((numberp f) (ncons (enode f)))
             ((null f) (ncons (enode f)))
             ((memq f invariables) (ncons f))
             ((memq f freevariables) 
              (setq invariables (cons f invariables))
              (ncons f))
             ((atom f) (ncons (enode f)))
             ((memq (car f) freevariables) 
              (perrormess2 '"Function symbol cannot be a free variable: " (car f)))
             ((atom (car f))
              (cons (length f)
                    (cons (enode (car f)) (mapcan 'prepattern (cdr f)))))
             (t (cons (length f) (mapcan 'prepattern f))))) 

(defuna paddrules (l) 
       (esave)
       (cond (perror (princ-start-terpri '"Rules not added."))
             ((addrules) 
              (princ-start-terpri '"Rules not added.") (erestore))
             (t (setq rulelist (nconc newrules rulelist))
                (princ-start-terpri '"Rules added.")))
       (setq constants constantnames)
       (setq functions functionnames)
       (setq predicates predicatenames)
       (setq freevariables nil)
       (setq newrules nil)
       '**noop**)

(defun ruleinit () 
       (prog (constants freevariables functions predicates)
             (mapcone (ruleinit1 (car x) (cadr x) (cddr x)) rulelist)))

(defun ruleinit1 (l lab rule)
       (setq constants (car l))
       (setq freevariables (cadr l))
       (setq functions (caddr l))
       (setq predicates (cadddr l))
       (addrule lab rule))

(defun rlistify (l)
       (cond ((atom l) l)
             ((eq (car l) ifexists)
              (list 'ifexists (plistify (car (last l))) (rlistify (car cursor))))
             ((eq (car l) ifthen)
              (list 'ifthen (plistify (car (last l))) (rlistify (car cursor))))
             ((eq (car l) ifthenelse)
              (list (car l) (plistify (car (last l)))
                    (rlistify (car (setq l (car cursor))))
                    (rlistify (cadr l))))
             ((eq (car l) merge*) 
              (setq l (car (last l)))
              (list 'merge* (rlistify (car l)) (rlistify (cdr l))))
             (t (mapcar 'rlistify l))))

(defun plistify (l)
       (cond ((numberp (car l))
              (prog (n)
                    (setq cursor (cdr l))
                    (setq n (car l))
                    (setq l nil)
                    (do i 0 (1+ i) (= i n) (setq l (cons (plistify cursor) l)))
                    (return (nreverse l))))
             ((atom (car l)) (setq cursor (cdr l)) (car l))
             ((isenode (car l)) (setq cursor (cdr l)) (eform (car l)))
             (t (break plistify))))

(defun printrules () (mapcone (sprinter (cddr x)) rulelist) 'ok)

(defuna psplitrule (lhs nil rhs)
        (prog (invariables f)
              (setq f (prepattern lhs))
              (cond ((instantiated rhs)
                     (rplacd (last f) (ncons (list splitrule* nil rhs)))
                     (ifexists nil nil nil f))
                    (t (perrormessrule nil '"Uninstantiated expression: " rhs)))
              (return '"**noop**")))

(mapcone (setq (x x)) '(splitrule*))

(defun splitrule* (node pmatchlist lab pattern) 
       (and slowmode
            (not (> (eheight node) pheightmax))))

(defun okheight (f)
       (cond ((atom f) t)
             ((isenode f) (not (> (eheight f) pheightmax)))
             (t (and (okheight (car f)) (okheight (cdr f))))))
