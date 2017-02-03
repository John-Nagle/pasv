
;;;(declare
;;;  (load 'need.o)
;;;  (load 'defmac.o)
;;;  (load 'enode.o)
;;;  (load 'princ.o)
;;;  (load 'map.o)
;;;  (load 'match.o)
;;;  (load 'progvn.o))

;;;(needs-macros)

(declarespecial 
	  boolsymand
          truesample falsesample simpflag propagateflag
          truenode falsenode trueprop falseprop historyprop simpprop
	  boolsymeq
	  boolsymimplies
	  boolsymnot
	  boolsymnoteq
	  boolsymor
	  demonnumber
	  eduplicatenumber
	  efirednumber
	  enumber
	  estats
	  falsenode
	  ifexistsnumber
	  ifthennumber
	  makemergenumber
	  poport
	  predicatenames
	  propagations
	  quotednamelist
	  quotednodelist
	  rareevents
	  restorerightrule
          rulenodelist
          ruletag
	  ruletime
	  slowmode
	  snegations
	  statswitch
	  truenode
	  zsymge
	  zsymgt
	  zsymle
	  zsymlt)

(defsmac mapeqclass
         (f r l)
         (prog (x finish) 
               (setq finish l)
               (setq x finish)
          a    f
	       (setq x (eqclass x))
               (and (eq x finish) (return r))
               (go a))) 

(defun s (f) (simp (normalize f)))

(defun simp (f)
       (setq truesample nil)
       (setq falsesample nil)
       (setq simpflag t)
       (setq trueprop '(true))
       (setq falseprop '(false))
       (setq historyprop nil)
       (setq propagateflag nil)
       (setq propagations nil)
       ((lambda (simpflag)
                (setq simpprop (ss (list f) nil nil nil nil nil)))
        nil)
       (cond ((and simpflag simpprop)
              (setq truesample nil)
              (setq falsesample nil)
              (esave)
              (cond ((eq (car f) boolsymimplies)
                     (turnonrules)
                     (startrule (caddr f))
                     (turnoffrules)))
              (setq simpprop (ss (list f) nil nil nil propagations nil))
              (erestore)))
       (cond (simpprop (list truesample falsesample))
             (t 'true)))

;
;	ss  --  propositional tableau routine
;
;	ss implements a tableau approach to propositional simplification.
;	All the Boolean operators are essentially analyzed by cases here.
;	
(defun ss (ff tt fff ttt ppp a)
       (cond (ff 
              (setq a (car ff))
              (setq ff (cdr ff))
              (cond ((atomp a)
                     (setq a (simpdeny a))
                     (cond ((null a) nil)
                           ((eq a t) (ss ff tt fff ttt ppp nil))
                           (t (ss ff tt fff ttt (append a ppp) nil))))
                    ((isenode a) nil) ; CL if a is an enode, don't try to take the car of it
                    ((eq (car a) boolsymnot)
                     (ss ff (cons (cadr a) tt) fff ttt ppp nil))
                    ((eq (car a) boolsymor)
                     (ss (cons (cadr a) (cons (caddr a) ff))
                         tt fff ttt ppp nil))
                    ((eq (car a) boolsymand)
                     (ss ff tt (cons a fff) ttt ppp nil))
                    ((eq (car a) boolsymimplies)
                     (ss (cons (caddr a) ff) (cons (cadr a) tt)
                         fff ttt ppp nil))
		    (t (setq a (simpdeny a))
                       (cond ((null a) nil)
                             ((eq a t) (ss ff tt fff ttt ppp nil))
                             (t (ss ff tt fff ttt (append a ppp) nil))))))
	     (tt
              (setq a (car tt))
              (setq tt (cdr tt))
              (cond ((atomp a)
                     (setq a (simpassert a))
                     (cond ((null a) nil)
                           ((eq a t) (ss ff tt fff ttt ppp nil))
                           (t (ss ff tt fff ttt (append a ppp) nil))))
                   ((isenode a) nil) ; CL if a is an enode, don't try to take the car of it
                   ((eq (car a) boolsymnot)
                     (ss (cons (cadr a) ff) tt fff ttt ppp nil))
                    ((eq (car a) boolsymor)
                     (ss ff tt fff (cons a ttt) ppp nil))
                    ((eq (car a) boolsymand)
                     (ss ff (cons (cadr a) (cons (caddr a) tt))
                         fff ttt ppp nil))
                    ((eq (car a) boolsymimplies)
                     (ss ff tt fff (cons a ttt) ppp nil))
		    (t (setq a (simpassert a))
                       (cond ((null a) nil)
                             ((eq a t) (ss ff tt fff ttt ppp nil))
                             (t (ss ff tt fff ttt (append a ppp) nil))))))
	    (fff
             (setq a (car fff))
             (setq fff (cdr fff))
             (simpsave)
             (cond ((ss (list (cadr a)) tt fff ttt ppp nil)
                    (simprestore)
                    t)
                   (t (simprestore)
                      (ss (list (caddr a)) tt fff ttt ppp nil))))
                     
	    (ttt
             (setq a (car ttt))
             (setq ttt (cdr ttt))
             (simpsave)
             (cond ((cond ((eq (car a) boolsymor)
                           (ss ff (list (cadr a)) fff ttt ppp nil))
                          (t (ss (list (cadr a)) tt fff ttt ppp nil)))
                    (simprestore)
                    t)
                   (t (simprestore)
                      (ss ff (list (caddr a)) fff ttt ppp nil))))
	    (ppp		; look at propagations after everything else
             (setq propagateflag t)
	     (rareevent "Used propagation")
             (simpsave)
             (setq a (ss ff ppp fff ttt nil nil))
             (simprestore)
             (setq propagateflag nil)
             (and a (setsample))
             a) 
	    (propagateflag t)
            (t (setsample) t)))

(defun setsample ()
       (cond 
;;; DISABLED DEBUG TRAP	     (truesample (break setsample))
             (simpflag 
              (mapeqclass (setq truesample (cons (sampleprint x) truesample))
                          nil truenode)
              (mapeqclass (setq falsesample (cons (sampleprint x) falsesample))
                          nil falsenode))
             (t (setq truesample trueprop) 
                (setq falsesample falseprop))))

(defun sampleprint (node)
       (cond ((atomp (esuccessors node)) (esuccessors node))
             (t (mapcar 'sampleprint (esuccessors node)))))

(defun simpassert (f)
       (cond (simpflag (vassert f))
             (t (assertprop f))))

(defun simpdeny (f)
       (cond (simpflag (deny f))
             (t (denyprop f))))

(defun simpsave ()
       (cond (simpflag (esave))
             (t (pushprop))))

(defun simprestore ()
       (cond (simpflag (erestore))
             (t (popprop))))

(defun assertprop (f)
       (cond ((member f trueprop) t)
             ((member f falseprop) nil)
             (t (setq trueprop (cons f trueprop)) t)))

(defun denyprop(f)
       (cond ((member f trueprop) nil)
             ((member f falseprop) t)
             (t (setq falseprop (cons f falseprop)) t))) 

(defun pushprop ()
       (setq historyprop (list trueprop falseprop historyprop)))

(defun popprop ()
       (setq trueprop (car historyprop))
       (setq falseprop (cadr historyprop))
       (setq historyprop (caddr historyprop))) 

; Rule Application. The rules will be pattern-matched before the simplifier
; itself is entered, but they will be applied at the leaves of the proof tree.

(defun startrule (f)
       (cond ((atomp f))
             ((eq (car f) boolsymnot) (startrule (cadr f)))
             ((memq (car f) (list boolsymand boolsymor boolsymimplies))
              (startrule (cadr f))
              (startrule (caddr f)))
             (t (enode f))))

(defun turnonrules () (setq turnon t) (mapc 'restorerightrule rulenodelist))

(defun turnoffrules () (setq turnon nil) (mapc 'rightrule rulenodelist))

(defun rightrule (x) (rplaca (car x) nil))

(defun restorerightrule (x) (rplaca (car x) (cdr x)))

; The following is the interface simpform and the e-graph.
;
;  (esave)      Save a copy of the egraph on a stack.
;  (erestore)   Restore the egraph from the top of the stack, and
;                  pop the stack.
;
;  (enode f)    Translate the formula f to an enode.
;  (eform p)    Translate the enode p to a formula.
;
;  (assert p)   Inform the e-graph that the enode p is true or false
;  (deny p)     (respectively).  Return a boolean indicating whether
;               this new knowledge generates an inconsistency.  If the
;               e-graph feels, on the basis of this new knowledge, some
;               case-splitting is needed, a formula will
;               onto propagations.  

(defun vassert  (f) 
       ((lambda (propagations)
                (catch 'falsetag (setq f (enode f))) ; reverse params for CL
                (cond ((eq propagations 'false) nil)
                      (t (catch 'falsetag (emerge truenode f)) ; reverse params for CL
                         (cond ((eq propagations 'false) nil)
                               (propagations)
                               (t)))))
        nil))

(defun deny (f) 
       ((lambda (propagations)
                (catch 'falsetag (setq f (enode f))) ; reverse params for CL
                (cond ((eq propagations 'false) nil)
                      (t (catch 'falsetag (emerge falsenode f)) ; reverse params for CL
                         (cond ((eq propagations 'false) nil)
                               (propagations)
                               (t)))))
        nil))

(defun propagate (f) 
       (cond ((eq f 'false) (setq propagations 'false) (throw 'falsetag 'false)) ; CL must name throw target
             (t (setq propagations (cons f propagations))
		(rareevent "Queued propagation"))))

(defun propagatefalse nil (setq propagations 'false) (throw 'falsetag 'false)) ; CL must name throw target

(defun epushenv nil (pushz)) 

(defun epopenv nil (popz)) 

(defunobj dequaltrue (node matchlist lab pattern) 
       (emerge (cdr (assq 'x matchlist)) (cdr (assq 'y matchlist)))) 

(defunobj dtruefalse (node matchlist lab pattern) (propagatefalse)) 

(defunobj dpropdemfalse (node matchlist lab pattern) (propagatefalse)) 

(defunobj dequalfalse (node matchlist lab pattern)
       (makemergedemon (cdr (assq 'x matchlist)) 
                       (cdr (assq 'y matchlist)) 
                       dpropdemfalse)) 
;
;	Theory of conses  --  not used in verifier
;
(defprop cons interncons intern) 

(defprop car interncar intern) 

(defprop cdr interncdr intern) 

(defun interncons (f) 
       (emerge (cadr (esuccessors f))
               (enode (list 'car f)))
       (emerge (caddr (esuccessors f))
               (enode (list 'cdr f)))
       f) 

(defun interncar (f) 
       (prog (x) 
             (setq x (cadr (esuccessors f)))
             (emerge x
                     (enode (list 'cons
                                           f
                                           (enode (list 'cdr x)))
                                     ))
             (return f))) 

(defun interncdr (f) 
       (prog (x) 
             (setq x (cadr (esuccessors f)))
             (emerge x
                     (enode (list 'cons
                                           (enode (list 'car x))
                                           f)
                                     ))
             (return f))) 


  (defun defineboolean () 
    ; This defines the boolean connectives that are known to the simplifier.
    ; Each connective is defined by calling definebool1; a nil argument
    ; means "none".
  
    (mapc #'(lambda (x) (definebool1 (car x) (cadr x)))
  
    ;   name used in           name used within
    ;  input language          the simplifier
    ;  ===============         ================
  
  	'(
  	  (and!			boolsymand)
  	  (or!			boolsymor)
  	  (implies!		boolsymimplies)
 ;	  (if!			boolsymcond)		;; NOT IMPLEMENTED
 ;	  (notimplies!		nil)			;; NOT IMPLEMENTED
 ;	  (impliedby!		nil)			;; NOT IMPLEMENTED
 ;	  (notimpliedby!	nil)			;; NOT IMPLEMENTED
  	  (not!			boolsymnot)
  	  (equal!		boolsymeq)
  	  (notequal!		boolsymnoteq))))
  
  (defun definebool1 (literal name)
    (setq predicatenames (cons literal predicatenames))
    (and name      (set name literal)))
;
;	Full initialization of entire simplifier
;
(defun simpinit nil 
       (setq rareevents nil)			; clear logging
       (prog nil a (or (popz) (go a)))
       (ereset)
       (zeinit)
       (setq propagations nil)
       (setq snegations (list (cons boolsymeq boolsymnoteq)
                              (cons boolsymnoteq boolsymeq)
                              (cons zsymle zsymgt)
                              (cons zsymgt zsymle)
                              (cons zsymlt zsymge)
                              (cons zsymge zsymlt)))
       (setq quotednodelist (mapcar 'quoteenode quotednamelist))
       (makemergedemon falsenode truenode dtruefalse)
       (makenodedemon truenode (list boolsymeq 'x 'y) dequaltrue '(x y))
       (makenodedemon falsenode (list boolsymeq 'x 'y) dequalfalse '(x y))
       (initbuiltin)
       (initgeneric)
       (pushcontext t)
       t) 

(defun quoteenode (x) (cons (enode (car x)) (cdr x)))

(defun makestats nil
   (list enumber ifexistsnumber makemergenumber ifthennumber 
      demonnumber efirednumber eduplicatenumber (simptime)))

(defun printstats (tme)
       (terpri)
       (princ '"nodes created: ") (princ-terpri enumber)
       (princ '"ifexists:      ") (princ-terpri ifexistsnumber)
       (princ '"makemerge:     ") (princ-terpri makemergenumber)
       (princ '"ifthen:        ") (princ-terpri ifthennumber)
       (princ '"demons:        ") (princ-terpri demonnumber)
       (princ '"demons fired:  ") (princ-terpri efirednumber)
       (princ '"duplicates:    ") (princ-terpri eduplicatenumber)
       (princ '"runtime:       ") (print-sec tme poport)
				  (princ '" sec.") 
       (terpri))
