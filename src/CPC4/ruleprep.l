;
;		Rule preprocessing
;
;		Patterns for rules are automatically generated
;		and unusable patterns are detected.  All this takes
;		place before the first proof is attempted.
;
;					Version 1.45 of 2/5/86
;
(declare (special
	accumpat
	eligibles
	errport
	freenames
	mapfunct
	pats
	rexpr
	rpat
	ruleexpr
	rulefile
	rulename
	rulestatus
	specialbuiltins
	varsneeded
	wantdummies
	vc-record
	))
;
;
;	mapsublist  --  apply function to all contained
;			sublists in arg
(defun mapsublist1 (x)				; internal fn for recursion
	(cond	((null x) nil)			; no action for nil
		((atom x) nil)			; no action for atom
		(t (funcall mapfunct x)		; apply fn to list
		   (mapc 'mapsublist1 (cdr x))))); map for list
(defun mapsublist (mapfunct x) (mapsublist1 x) nil)	; outer fn for binding fn
;
;	sunion  --  union of sorted unique lists of atoms
;
(defun sunion (x y)
	(cond 	((null x) y)			; null x case
	     	((null y) x)			; null y case
	        ((equal (car x) (car y)) (sunion (cdr x) y)) ; avoid dupl
		((alphalessp (car x) (car y)) (cons (car x) 
						(sunion (cdr x) y)))
		(t (cons (car y) (sunion x (cdr y))))))
;
;	sintersection  --  intersection of sorted unique lists of atoms
;
(defun sintersection (x y)
	(cond 	((null x) nil)			; null x case
	     	((null y) nil)			; null y case
	        ((equal (car x) (car y)) 
			(cons (car x) (sintersection (cdr x) (cdr y))))
		((alphalessp (car x) (car y)) (sintersection (cdr x) y))
		(t (sintersection x (cdr y)))))
;
;	sdifference  --  difference of sorted unique lists of atoms
;
(defun sdifference (x y)
	(cond	((null x) nil)			; null x case
		((null y) x)			; null y case
		((equal (car x) (car y))	; equal, not in difference
			(sdifference (cdr x) (cdr y)))
		((alphalessp (car x) (car y))	; x less, advance x
			(cons (car x) (sdifference (cdr x) y)))
		(t (sdifference x (cdr y)))))	; y less, advance y
;
;	bigsunion  --  union of list of sets
;
(defun bigsunion (x)
	(cond	((null (cdr x)) (car x))	; if last elt, use
		(t (sunion (car x) (bigsunion (cdr x)))))) ; otherwise recurse
;
;	initialization of the list of functions illegal in patterns
;
(defun   illegalinpattern (fn) (putprop fn t 'nopattern))
(mapcar 'illegalinpattern '(
     addi!	and!		assign!		consti!		divi!
     divide!	equal!		false		gei!		gti!
     if!	impliedby!	implies!	lei!		lti!
     negi!	not!		notequal!	notimpliedby!	notimplies!
     or!	subi!		true		addn!		subn!
     gtn!	addn!		subn!
     ))
;
;	specialbuiltins  --  builtins without demons, good in patterns
;
(setq specialbuiltins '(arraytrue!))
;
;	builtinp  --  true if built-in prover name
;
(defun builtinp (a)
    (cond	((not (atom a)) nil)		; must be atom
		((member a specialbuiltins) nil); not builtin if special
		((equal '! (car (last (explode a)))) t) ; ends in !
		((eq a 'true) t)		; not true constant
		((eq a 'false) t)		; not false constant
		(t nil)))			; not builtin
;
;	freevar  -- is a variable in a rule a free variable?
;
;	Names ending with "!" are not free variables.
;	Neither are numbers.
;	A list of free variables is constructed.
;
(defun freevar (a)
    (cond	((not (atom a)) nil)		; only atoms are free
		((null a) nil)			; nil is not a free var
		((numberp a) nil)		; numbers are not free
		((builtinp a) nil)		; builtins are not free
		((and (not wantdummies) (isdummy a)) nil) ; dummies not free
		((member a freenames) nil)	; already known
		(t (setq freenames (append1 freenames a)))))
;
;	findfree  --  find free variable in single function call
;
;	An atom is a variable, and a list is a function call.
;
(defun findfree (e)
	(cond	((atom e) (freevar e))		; handle atomic case
		((null (cdr e)))		; quit if no args
		((mapc 'findfree (cdr e))))	; apply findfree to args
	)
;
;	freevars  --  find free vars in expression, and return a
;		      list of them.
;
(defun freevars (e)
    (prog (freenames wantdummies)		; will return names
	(setq wantdummies nil)			; no dummies
	(findfree e)				; find free vars
	(return (sort freenames 'alphalessp))))	; and return them

;
;	freeanddummyvars  --  find free vars in expression, and return a
;		      	      list of them.  But include dummies.
;
(defun freeanddummyvars (e)
    (prog (freenames wantdummies)		; will return names
	(setq wantdummies t)			; we want dummies
	(findfree e)				; find free vars
	(return (sort freenames 'alphalessp))))	; and return them
;
;	Automatic pattern generation
;
;
;	eligiblefn  --  is fn eligible to appear in pattern?
;
;	Ineligible functions (and!, gti!, etc. are so tagged).
;
(defun eligiblefn (fn)				; function to test
	(cond	((not (atom fn)) nil)		; must be atom
		((get fn 'nopattern) nil) 	; ineligible built in
		(t t)))
;
;	insertdummies  --  insert dummy free variables for
;			   impermissible subexpressions
;
(defun dummyname () 
	(prog (newsym)
		(setq newsym (gensym))		; get a dummy
		(putprop newsym t 'dummyname)	; tag as dummy
		(return newsym)))		; return dummy

(defun insertdummies (exp)
	(cond	((atom exp) exp)		; atom returns self
		((null (car exp)) exp)		; null returns self
		((eligiblefn (car exp))		; if eligible, recurse
			(cons (car exp) (mapcar 'insertdummies (cdr exp))))
		(t (dummyname))))		; otherwise dummy 
;
;	isdummy  --  is this a dummy?
;
(defun isdummy (name)
	(get name 'dummyname))			; t if dummy
;
;	eligibleexprs  -- returns a list of expressions
;			 eligible to become patterns
;
(defun addeligible (exp)			; add new possible pattern
	(cond	((member exp pats) nil)		; avoid duplicate
		(t (setq pats (append1 pats exp)))))	; add to list
(defun eligibleexpr (f)				; check for eligibility
	(cond 	((atom f) nil)			; must be list
		((null (car f)) nil)		; must have head
		((not (eligiblefn (car f))) nil); must be eligible
		(t (addeligible (insertdummies f))))) ; add to list
(defun eligibleexprs (exp)
	(prog	(pats)				; possible patterns
		(mapsublist 'eligibleexpr exp)	; process subexprs
		(return pats)))
;
;	heuristicsort  --  sort by arbitrarily-assigned goodness measure
;			   Note that this sort is order-preserving.
;
(defun heuristicsort (patterns)		; make (value pattern) list, and sort
    (mapcar 'cdr (sort (mapcar 'assignvalue patterns) 'valuecomp)))

(defun valuecomp (x y)				; compare value cell
	(greaterp (car x) (car y)))		; sort by value cell

(defun assignvalue (x)				; make (value pattern) elt
	(cons (heuristicvalue x) x))
;
;	heuristicvalue  --  measure of goodness as pattern
;
;	Long patterns are better than short patterns.
;	Patterns beginning with user functions are better than
;	ones beginning with non-builtin functions.
;
(defun heuristicvalue (x)
	(plus (cond ((builtinp (car x)) 0) (t 100)) (length (freevars x))))
;
;	patternorder  --  order possible patterns by value
;
(defun patternorder (x)
	 (heuristicsort (eligibleexprs x)))
;
;	patternset  --  build set of patterns which will bind all
;			free variables in formula
;
;	Heuristics are used here.  They are:
;	1.  For an implies! rule, patterns from the right side of the
;	    implication are used before patterns from the left side, and
;	    there must be at least one right side pattern.
;
(defun patternset (exp)
    (prog 	(eligibles varsneeded accumpat lhspat rhspat)
	(setq varsneeded (freevars exp))	; get list of free vars
						; get eligible exprs
	(cond ((eq (car exp) 'implies!)		; if implication rule
		(setq lhspat (patternorder (cadr exp))) ; get lhs pats
		(setq rhspat (patternorder (caddr exp)))	; get rhs pats
		(cond ((null rhspat)		; if no rhs
			(ruleerror "conclusion of IMPLIES is too common a form")
			(return nil)		; no expression
			))
		(setq eligibles (append rhspat lhspat)) ; construct list
		)
	      (t (setq eligibles (patternorder exp)))) ; non-implies rule
	(patterntest)				; test all eligibles
	(cond ((null varsneeded) (return accumpat))) ; success
	(patom "WARNING: Rule " errport)
	(patom rulename errport)
	(patom " ignored: " errport)
	(terpri errport)
	(patom "    The rule handler cannot figure out how to bind " errport)
	(printnamelist varsneeded)		; print the list
	(patom "." errport)
	(terpri errport)
	))
;
;	fntest  --  is function present in expr?
;
(defun fntest (fn expr)
	(cond	((null expr) nil)	; dull case
		((listp expr)
			(or (equal fn (car expr)) ; find?
			    (fntest fn (cdr expr)))) ; recurse on other args
		(t nil)))
;
;	eligfntest  --  is function present in eligible list?
;
;	The input is a list of exprs
;
(defun eligfntest (fn exprs)
	(cond	((null exprs) nil)	; dull case
		((listp exprs) (or (fntest fn (car exprs)) ; do the work
			(eligfntest fn (cdr exprs))))
		(t nil)))

;
;	Patterntest  --  test if pattern relevant
;
;	A pattern is relevant if it binds another variable, or if
;	it begins with a non-builtin function.  This last is valid
;	because no demon can create a non-builtin, and rules are
;	applied only one deep.
;	
(defun patterntest nil				; try to find enough patterns
    (prog (v)
        (cond	((null eligibles) (return nil))); end of list, stop
	(setq v (freevars (car eligibles)))	; get free vars this pattern
	(cond 	((sintersection v varsneeded)	; if pattern has needed vars
						; add pattern
		    (setq accumpat (append1 accumpat (car eligibles))) 
		    (setq varsneeded (sdifference varsneeded v)) ; reduce need
		)
		((and (not (builtinp (caar eligibles))) ; if user-defined fn
						; appearing for 1st time
		      (not (eligfntest (caar eligibles) accumpat))) 
						; always take
		        (setq accumpat (append1 accumpat (car eligibles))))
		)
	(setq eligibles (cdr eligibles))	; on to next pattern
	(patterntest)				; and recurse
	))
(defun printnamelist (lst)			; print nice list
    (cond ((null lst) nil)			; if null, exit
	  ((atom lst) (patom lst errport))	; atom prints as self
	  ((null (cdr lst)) (patom (car lst) errport))
	  ((null (cddr lst)) (patom (car lst) errport) 
			(patom " and " errport)
			(patom (cadr lst) errport))
	  (t (patom (car lst) errport) (patom ", " errport)
		(printnamelist (cdr lst))) ; non-last
	))
;
;	validaterule  --   make sure a rule is valid, diagnosing if not
;
(defun validaterule (expr free pat)
    (prog (err)
	(cond ((null expr) (setq err "Empty rule."))
	      ((null free) (setq err "No free variables in rule."))
	      ((null pat)  (setq err "No good way to decide when to use rule."))
		)
	(cond ((null err) (return t)))		; success
	(ruleerror err)				; output message
	))
(defun ruleerror (errmsg)			; start error message
	(patom "WARNING: Rule " errport)
	(patom rulename errport)
	(patom " ignored: " errport)
	(terpri errport)
	(patom "    " errport)
	(patom errmsg errport)
	(terpri errport)
	)
;
;	newrule  --  add a new rule, generating pattern and free variable set.
;
(defun newrule (rulename rexpr)
    (prog (rpat rfree isright)
	(cond ((null rexpr) (return nil)))	; if null, fails
	(setq rfree (freevars rexpr))		; get the free vars
	(setq rpat  (patternset rexpr))		; get the pattern
	(cond ((validaterule rexpr rfree rpat) 	; validate the rule
		(setq rfree (bigsunion (mapcar 'freeanddummyvars rpat)))
		(setq isright (< (fnsinpat (car rpat)) 2)) ; recog good rules
		(cond (isright (addrightrule rpat rexpr rfree rulename))
		      (t	(addrule rpat rexpr rfree rulename)))
		(cond ((portp vc-record)	; if logging is on
			(patom rulename vc-record)	; print name
			(patom " --  Usable " vc-record)
			(patom 
			    (cond (isright "on conclusions only")
			          (t "anywhere")) vc-record)
			(patom ", free variables " vc-record)
			(print rfree vc-record)	; print free vars
			(terpr vc-record)
			(patom "Trigger pattern sequence: " vc-record)
			(print rpat vc-record)	; print patterns
			(terpr vc-record)
			(terpr vc-record)
			(pform rexpr vc-record)	; print the rule
			(terpr vc-record)
			(terpr vc-record)))
		(return t))
	      (t (setq rulestatus nil) nil))	; fails
	))
;
;	fnsinpat  --  count number of function calls in pattern
;
(defun fnsinpat (f)
	(cond ((null f) 0)			; null has value 0
	      ((atom f) 0)			; atom has value 0
	      ((null (cdr f)) 0)		; no args, value 0
	      (t (+ 1 (fnsinpat1 (cdr f))))))	; args, 1 + value of args
(defun fnsinpat1 (f)
	(cond ((null f) 0)			; null has value 0
	      ((atom f) 0)			; atom has value 0
	      (t (+ (fnsinpat1 (cdr f)) (fnsinpat (car f)))))) ; recurse
