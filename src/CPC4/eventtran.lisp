"@(#)eventtran.l	1.27"
;
;	Translator for Boyer-Moore events.
;
;	Files of Boyer-Moore events are read and translated into
;	simplifier rules.
;
(declarespecial
	xlateerror
	nametail1
	nametail2
	boolsymor
	boolsymand
	boolsymeq
	boolsymimplies
	boolsymnot
	integertype
	booleantype
	errport
	)
;
;	Table of Boyer-Moore symbol translations
;
;	Entries of the form
;	  ('NAME 'name 'translatefnname)  cause name translation
;
;	Entries of the form
;	  ('NAME 'fn 'specialtranslate)   cause invocation of fn
;
;	Invocation of 'diagnosesymbol is used for illegal names.
;
(defun setxlate (lst)			; set translation info
	(putprop (car lst) (cadr lst) (caddr lst)))
(mapcar 'setxlate '(
	(t true translatevarname)
	(f false translatevarname)
	(equal equalxlate specialtranslate)
	(if diagnosesymbol specialtranslate)
	(not not! translatefnname)
	(and and! translatefnname)
	(or or! translatefnname)
	(implies implies! translatefnname)
	(lessp lesspxlate specialtranslate)
	(add1 add1xlate specialtranslate)			
	(zero 0 translatevarname)
	(numberp numberp! translatefnname)		
	(sub1 sub1xlate specialtranslate)			
	(sub1p diagnosesymbol specialtranslate)
	(pack diagnosesymbol specialtranslate)
	(litatom diagnosesymbol specialtranslate)
	(unpack diagnosesymbol specialtranslate)
	(unpackp diagnosesymbol specialtranslate)
	(cons diagnosesymbol specialtranslate)
	(listp diagnosesymbol specialtranslate)
	(car diagnosesymbol specialtranslate)
	(cdr diagnosesymbol specialtranslate)
	(nil diagnosesymbol specialtranslate)
	(zerop zeropxlate specialtranslate)
	(plus addn! translatefnname)
	(difference subn! translatefnname)
	(times muln! translatefnname)
	(quotient divn! translatefnname)
	(remainder mod! translatefnname)
	(lti! ltixlate specialtranslate)
	(lei! leixlate specialtranslate)
	(and translatenary specialtranslate)
	(and! translatenary specialtranslate)
	(or translatenary specialtranslate)
	(or! translatenary specialtranslate)
	))
;
;	Translators for special cases
;
;	LESSP, lti! and lei!  --  only gti!, gtn!, and gei! are allowed.
;
(defun ltixlate (s)		; swap args and change operator
	(list 'gti! (translaterule1 (caddr s)) (translaterule1 (cadr s))))	
(defun leixlate (s)
	(list 'gei! (translaterule1 (caddr s)) (translaterule1 (cadr s))))	
(defun lesspxlate (s)
	(list 'gtn! (translaterule1 (caddr s)) (translaterule1 (cadr s))))	
;
;	ZEROP  --  translate to (equal! n 0)
;
(defun zeropxlate (s)
	(list 'equal! (translaterule1 (cadr s)) 0))
;
;	EQUAL  --  if both operands are Boolean, generate case expression
;
;	In other words, (EQUAL  (F X) Y)
;	will become
;			(OR (AND (F X) Y) (AND (NOT (F X)) (NOT Y)))
;	although as a special case we recognize (EQUAL X T) and (EQUAL T X)
;	and just generate X.
;
(defun equalxlate (f)
  (prog (a b)
	(setq a (translaterule1 (cadr f)))	; translate to normalized
	(setq b (translaterule1 (caddr f)))	; internal form
	(return (cond ((and (isbmboolean a) (isbmboolean b))
		       (booleanequality a b)) ; if boolean, do casing
		      (t (list boolsymeq a b)))))) ; otherwise use equal
;
;	ADD1  --  translate to addn! with 1
;
(defun add1xlate (s)
	(list 'addn! (translaterule1 (cadr s)) 1))
;
;	SUB1  --  translate to subn! with 1
;
(defun sub1xlate (s)
	(list 'subn! (translaterule1 (cadr s)) 1))
;
;	translatenary  --  convert N-ary operators to binary operators
;
;	Used on Boyer-Moore AND and OR operators.
;
(defun translatenary (s)
    (cond ((eq (length s) 3) 		; if 2-operand form
		(list (car s) 		; just recurse, avoiding loop
			(translaterule1 (cadr s)) 
			(translaterule1 (caddr s))))
	  ((lessp (length s) 3) (diagnosesymbol (car s))) ; error if too small
	  (t	(list (car s) 		; more than 2 op causes breakapart
		      (translaterule1 (cadr s))
		      (translaterule1 (cons (car s) (cddr s)))))
	  ))
;
;	diagnosesymbol  --  bad symbol in Boyer-Moore rule
;
(defun diagnosesymbol (s)
    (setq xlateerror t)		; note failure
    (cond ((atomp s) (ruleerror (concat "Illegal " s " in rule.")))
	  (t (diagnosesymbol (car s)))))
;
;	translaterule  --  translate a rule into standard form
;
;	Boyer-Moore operators are translated to simplifier operators,
;	and some operators get special processing.
;
;	If an error is detected, nil is returned, and this propagates up.
;
(defun translaterule1 (s)		; inner translate
    (prog (xl op)
	(cond 	((symbolp s)			; if symbol, translate var name
					(return (cond ((get s 'translatevarname)) (t s))))
				((atomp s) (return s)))			; non-symbol atom, no translation
	(setq op (car s))		; function name (operator)
					; translate function name
	(cond ((setq xl (get op 'translatefnname))
		(setq s (cons xl (cdr s)))))
					; special handler if indicated
        (cond ((setq xl (get op 'specialtranslate)) 
	    (return (funcall xl s))))	; special handler must recurse
					; descend recursively
	(return (cons (car s) (mapcar 'translaterule1 (cdr s))))))
(defun translaterule (s)		; main translate
    (prog (xlateerror)			; if error found
	(setq s (translaterule1 s))	; translate the rule
	(return (cond ((null xlateerror) s)	; if no error, return s
	       (t nil)))))		; if fail, return nil
;
;	rule loading  --  loads rules from the filename given
;
(defun loadrules (rulefile)
    (prog (rulestatus ruleport)			; changed globally if fail
	(cond ((not (probef rulefile)) (return t)))	; no file is OK
	(setq rulestatus t)			; assume OK
	(setq ruleport (infile rulefile))	; open the file
	(readevents ruleport)			; read the events
	(close ruleport)			; done with rule port
	(return rulestatus)			; return reported status
	))
;
;	event reader  --  reads a file of Boyer-Moore events
;
;	Events are processed by a handler for the appropriate type of event.
;	At present, only PROVE.LEMMA events are considered.
;
(declarespecial event inport)			; global
(defun readevents (inport)			; read all events
    (prog (event)
	(nextevent)				; process chain 
	))
(defun nextevent nil				; process the next event
    (prog nil
    	(setq event (read inport))		; read an event
    	(cond ((null event) (return nil)))
	(cond ((atomp event) nil)		; event dispatcher
	      ((eq (car event) 'prove-lemma) (dolemma event)) ; lemma
	      ((eq (car event) 'defn) (dodefn event)) ; definition
	      ((eq (car event) 'add-axiom) (doaxiom event)) ; axiom
	      (t nil))				; not interesting type
	(nextevent)))				; recurse to next event
;
;	dolemma  --  process a PROVE.LEMMA
;
;	A rule will be generated and installed.
;
(defun dolemma (event)				; process an lemma
    (prog (ename ekinds eexpr)
	(typelemma event)			; check for typing lemma
	(setq ename (cadr event))		; name of event
	(setq ekinds (caddr event))		; (REWRITE, etc.)
	(setq eexpr (cadddr event))		; the lemma
	(cond ((not (member 'rewrite ekinds)) (return nil)) ; must be REWRITE
	      ((not (goodrulename ename)) (return nil)) ; must end .RULE
	       (t nil))
	(setq eexpr (translaterule eexpr))	; translate notation
	(cond ((null eexpr) (return nil)))	; fails if null
	(newrule ename eexpr)			;  load the rule
	))
;
;	dodefn  --  process a DEFN
;
;	For each DEFN, we record the fact that it is a DEFN and
;	the type of the DEFN, if determinable from the definition alone.
;
(defun dodefn (defn)
	(putprop (cadr defn) t 'defn)		; just mark
	(putprop (cadr defn) (infertype (cadr defn) (cadddr defn)) 'defntype))
;
;	infertype  --  infer what the type of a expression is from various info
;
;	Since Boyer-Moore has untyped syntax, this is not always determinable.
;	We need to know, though, in two cases - in a DEFN, we need to check
;	against the definition in the program, and for expressions within
;	EQUAL, we need to generate a Boolean expression to cause case splitting.
;
;	The only types which may be returned are 
;		(integer)
;		(boolean)
;		nil -- when type not determinable as one of above
;
;	Note: this operates on Boyer-Moore syntax.
;
(defun infertype (defnname expr)
    (prog (op t1 t2)
	(cond ((null expr)	(return nil)))	; if null, fails
	(cond ((atomp expr)			; if atom
		(cond ((numberp expr) (return integertype))	; number
		      ((equal expr 't) (return booleantype))	; True
		      ((equal expr 'f) (return booleantype)) 	; False
		      ((equal expr 'true) (return booleantype))	; True
		      ((equal expr 'false) (return booleantype)); False
		      (t (return nil)))))	; other atoms unknown
	(setq op (car expr))			; is list, get operator
	(setq t1 (getbmoptype op))		; look up type
	(and t1 (return t1))			; if found, good
	(cond ((equal op 'if)			; if IF
		(setq t1 (infertype defnname (caddr expr))) ; examine opr 2
		(setq t2 (infertype defnname (cadddr expr))) ; examine opr 3
		(and (equal t1 t2) (return t1))	; success if same
		; recursion check -- if arg of IF is recursive call,
		; use type on other branch.  Valid because recursion must
		; terminate by the Boyer-Moore definitional principle.
		; There is no mutual recursion in Boyer-Moore, of course.
		(and (not (atomp (caddr expr)))	; if call
		     (equal (caaddr expr) defnname) ; and recursive call
		     (return t2))		; use other type
		(and (not (atomp (cadddr expr))) ; if call
		     (equal (caadddr expr) defnname) ; and recursive call
		     (return t1))		; use other type
		(return nil))			; fails if nil
	      (t (return nil)))))		; fail, return
;
;	getbmoptype  --  get type given Boyer-Moore function symbol
;
(defun getbmoptype (op)
    (cond ((get (get op 'translatefnname) 'optype)) ; look for BM builtins
	  ((get op 'optype))			; try Oppen builtins
	  (t (get op 'defntype))))		; otherwise try DEFNs
;
;	isbmboolean  --  test expression for booleanness
;
;	If unknown, nil is returned.
;
(defun isbmboolean (expr)
    (equal (infertype nil expr) booleantype)) ; true if boolean
;
;	typelemma  --  note a type lemma
;
;	Type lemmas unconditionally state that a given DEFN
;	is of a specific type.  The form of such a lemma must be
;	
;	    (integerp! (<defnname> <arg1> <arg2> ...))
;	or
;	    (booleanp! (<defnname> <arg1> <arg2> ...))
;
;
(defun typelemma(event)
    (prog (ename ekinds eexpr dtype efn)
	(setq ename (cadr event))		; name of event
	(setq ekinds (caddr event))		; (REWRITE, etc.)
	(setq eexpr (cadddr event))		; the lemma
	(setq dtype
	  (cond ((atomp (cadr eexpr)) nil); not predicate of call
	      ((not (allatomic (cadr eexpr))) nil) ; non-builtin
	      ((equal (car eexpr) 'booleanp!) booleantype)
	      ((equal (car eexpr) 'integerp!) integertype)
	      (t nil)))				; others fail
	(and (null dtype) (return nil))		; if no type, fail
	(putprop (caadr eexpr) dtype 'defntype); record type of defn
	))
;
;	allatomic  --  check for all elts simple free vars
;
(defun allatomic (lst)
    (cond ((null lst) t)			; if empty, done
	  ((not (atomp (car lst))) nil)		; if not atom, fail
	  ((member (car lst) '(T F)) nil)	; if constant, fail
	  ((numberp (car lst)) nil)		; if constant, fail
	  (t (allatomic (cdr lst)))))		; otherwise recurse

;
;	doaxiom  --  process an ADD.AXIOM
;
;	ADD.AXIOM is a nono.  We do not allow the introduction of unsoundness.
;
(defun doaxiom (axiom)
	(patom "FATAL ERROR: Forbidden command ADD.AXIOM(" errport)
	(patom (cadr axiom) errport)
	(patom ") used making rule data base!" errport)
	(exit 1)				; fails
	)
;
;	goodrulename  --  test rule name to see if it is a good one.
;
;	Rule names must end in "-rule" or "-RULE" to be accepted.
;
(defun goodrulename1 (name)
    (prog (nametail)				; tail part of name
	(setq nametail (member #\- name))	; get part after dot
	(cond ((null nametail) (return nil))	; failure - end of string
	      ((equal nametail nametail1) (return t)) ; success
	      ((equal nametail nametail2) (return t)) ; success
	      (t (return (goodrulename1 (cdr nametail))))); if more, recurse
	))
(defun goodrulename (n) (goodrulename1 (explodec n))) ; start recursion
						; canned name tails
(setq nametail1 (explodec '\-RULE))		; name tail 1
(setq nametail2 (explodec '|-rule|))		; name tail 2
