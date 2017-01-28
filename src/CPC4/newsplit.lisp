"@(#)newsplit.l	2.2"

; This file contiains the functions that perform new splitting.  The
; entry point is the function:
; 
;    (newsplit 'varlist 'formula)
; 
; where varlist is a list of atoms, and formula is a J-code formula.
; The input represents a new instruction.  At this point in the processing,
; occurrences of (new! x) will have been replaced by decorations at the
; end of the variable; the symbols in varlist should be decorated in the
; same fashion.
; 
; Newsplit breaks the new into one or more equivalent instructions, and
; sends the result back over the port to-vcg.  The format used to send a
; new back is (1) a list of variables (without parentheses), one to a
; line, followed by the formula, all on one line.  This is unambiguous,
; because the formula always begins with a paren, and variables never
; begin with a paren.  A series of new instructions are sent; the end of
; the expressions is signalled by a line containing a slash.

;;;(declare (load 'need.o) (load 'hunkshell.o) (load 'match.o))
;;;(needs-macros)

(declarespecial newvars to-vcg)

(defun newsplit (newvars form)
  ; New spliting works as follows.  First an equivlence relation is set up
  ; whose elements are the variables that are being newed and all the
  ; conjuncts of the formula.  The relation is coarsened using a union-find
  ; algorithm.  Each conjunct is scanned, all the newed variables are
  ; located, and the equivalence class containing the conjuct is merged
  ; into the equivalence class containing each of its newed variables.
  ; After all the conjucts have been scanned, a new instruction is
  ; generated for each equivalence class.  The variables in the class form
  ; the variable list, and all the conjucts in the list are joined by and
  ; to form the formula.

  (mapc 'atom-class newvars)  ; make sure each variable gets into a class
  (and-split form)            ; scan each conjunct
  (enum-classes)              ; enumerate the classes
  (erase-classes)             ; clean up for the next call
  nil)                        ; and return nil

(defun and-split (f)
  ; Traverse the formula f, locating all the conjuncts;
  ; call call splitscan on each.
  (cond ((atomp f) (splitscan f))
	((eq (car f) 'and!) (mapc 'and-split (cdr f)))
	(t (splitscan f))))

(declarespecial scanroot)

(defun splitscan (x)
  ; create an element for the formula x, find all the varaibles in x,
  ; and merge them into the class containing x.
  (prog (scanroot)
	(setq scanroot (new-splitelt (simp-assign x)))
	(findvars x)))

(defun findvars (x)
  ; Find all the variables in x and merge them into scanroot
  (cond ((atomp x))
	((atomp (car x))
	 (and (memq (car x) newvars) (smerge (atom-class (car x)) scanroot))
	 (mapc 'findvars (cdr x)))
	(t (mapc 'findvars x))))

(defun simp-assign (x)
  ; Translate all subformulas of the form (assign! (v) E),
  ; where v is an atom, to E.  The simplifier does this already,
  ; but it is necessary, since the redundant presence of (v)
  ; causes it to count as a live variable and drag in more assertions. 

  (prog (A E)
	(return (cond ((atomp x) x)
		      ((and (match x ('assign! (A) E))
			    (atomp A))
		       (simp-assign E))
		      (t (simp-assign-list x))))))

(defun simp-assign-list (x)
  (cond ((null x) x)
	(t (rplaca x (simp-assign (car x)))
	   (rplacd x (simp-assign-list (cdr x)))
	   x)))

; The data structure used to maintain equivalence classes
(hunkshell splitelt ; element of equivalence newsplit equivalence class
	   sval     ; the object represented by this class
	   sroot    ; the first element of the class
	   slink    ; field used to link all elements of the calss
	   ssize    ; number of elements in the class
		    ; (only valid for roots)
	   savail   ; field used to link all existing splitelts
	   )

; Following the practice used for enodes, once a splitelt is
; allocated, it is never returned.  All the used and unused
; splitelts are kept on lists linked through the savail field.

(declarespecial unused-head ; A list of unused splitelts
		  used-head   ; A list of used splitelts
		  )

(setq unused-head nil)
(setq used-head nil)

(defun new-splitelt (v)
  ; create a new equivalence class with value v
  (prog (n)
	(cond ((null unused-head)
	       
	       ; We are out of splitelts; make a new one.
	       (setq n (alloc-splitelt)))
	      
	      (t ; get the next element from the unsued list
		 (setq n unused-head)
		 (setq unused-head (savail n))))
	
	; Add n to the list of splitelts in use
	(xsavail n used-head)
	(setq used-head n)

	; set up the fields of n to be a new equivalence class
	(xsval n v)
	(xsroot n n)
	(xslink n nil)
	(xssize n 1)
	(return n)))

(defun atom-class (a)
  ; return the splitelt for the atom a, or create one if none exists
  (cond ((get a 'splitelt))
	((putprop a (new-splitelt a) 'splitelt))))

(defun smerge (x y)
  ; x and y must be splitelts.  Merge the classes containing x and y.
  
  (prog (l)
	; We work with the roots of the equivalence classes
	(setq x (sroot x))
	(setq y (sroot y))
	
	; If x and y are already equivalent, we are done
	(and (eq x y) (return nil))
	
	; make (size y) >= (ssize x) by swapping x and y, if necessary
	(and (> (ssize x) (ssize y))
	     (prog (s)
		   (setq s x)
		   (setq x y)
		   (setq y s)))
	
	; Scan the list beginning with x and make all the roots point
	; at y.  Set l to the last element scanned.
	(do (j x (slink j)) (null j)
	    (xsroot j y)
	    (setq l j))
	
	; Insert the list beginning with x just after the first element
	; of the list beginning with y.
	(xslink l (slink y))
	(xslink y x)
	
	; Update the size field of y to reflect the elements gained from x
	(xssize y (+ (ssize x) (ssize y)))))

(defun erase-classes ()
  ; Transfer every element on the used-list to the unused-list.
  ; Along the way, remove these elements from any property list they are on.

  (do ((s)) ((null used-head))
      (and (atomp (sval used-head)) (remprop (sval used-head) 'splitelt))
      (xsval used-head nil) ; give the value back to the garbage collector
      (setq s used-head)
      (setq used-head (savail used-head))
      (xsavail s unused-head)
      (setq unused-head s)))

(defun enum-classes ()
  ; Enumerate the equivalence classes, and write on to-vcg the
  ; new instruction represented by each.
  
  ; Step j through every splitelt, looking for roots
  (do (j used-head (savail j)) (null j)
      (and (eq j (sroot j))
	   (prog (and-list v v-count)
		 (setq v-count 0)
		 (setq and-list '(true!))
		 
		 ; Step k through every element of the equivalence class
		 ; If k is a variable, print it out.  If k is a formula,
		 ; add it onto and-list as a conjunct.
		 (do (k j (slink k)) (null k)
		     (setq v (sval k))
		     (cond ((atomp v)
			    (setq v-count (1+ v-count))
			    (patom v to-vcg) (terpr to-vcg))
			   (t (setq and-list
				    (cond ((equal '(true!) and-list) v)
					  (t (list 'and! v and-list)))))))
		 
		 ; Now print the accumulated formula.  As a special case,
		 ; do not print (true!) if no variables were printed.
		 (cond ((not (and (= v-count 0)
				  (equal '(true!) and-list)))
			(print and-list to-vcg) (terpr to-vcg))))))
  
  ; Finally, print a marker to indicate we are done.
  (patom '|/| to-vcg) (terpr to-vcg))
