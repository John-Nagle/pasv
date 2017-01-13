
(declare
  (load 'need)
  (load 'defmac)
  (load 'progvn)
  (load 'match))

(needs-macros)

(declare (special 
	  predicatenames
	  boolsymeq
	  boolsymimplies
	  boolsymnot
	  boolsymnoteq
	  boolsymor
	  boolsymand
	  ))

;
;	Normalization preprocessing
;
;					Version 2.9 of 6/1/83
;
; Normalization is performed on incoming VCs before they are submitted
; to the prover proper.
; The normalizing function performs the following tasks:
;
; 1)  Several simple translations from Jcode notation to simplifier
;     notation are performed.
;
; 2)  If (f a b c ...) is a call to a user function f, the
;     normalizer inserts type information, translating the formula into the
;     form (type! f a b c ...).  Note that user variables are encoded as
;     functions with no operands.  Type information is also inserted before
;     the operators selecta! selectr! storea! and storer!.
;
; It should be noted that the output of normalize may not be fed to normalize.
; Normalize will not process expressions containing "typed!"
;
(defun normalize (f) (normalize1 f))
;
;	normalize1  --  inner recursive normalize
;
(defun normalize1 (f)
  (prog (fun) 
	(return (cond
		 ; atoms and enodes normalize to themselves
		 ((atom f) f)

		 ; if f is of the form (g ...), check if a normalizing
		 ; function has been provided for g.  If so, use it.

		 ((and (atom (car f))
		       (not (numberp (car f)))
		       (setq fun (get (car f) 'normalize)))
		  (funcall fun f))
		 
		 ; Otherwise, perform the "default" normalization, which is
		 ; a type association only.

		 (t (addtype f))))))

(defun addtype (f)
  ; This function tests if the expression f is of the form (s ...), where s
  ; does not end in !.  If f is not of that form, the function simply returns
  ; f.  
  ; Expressions which are of the form (s) represent simple variables, and
  ; s is expected to be of the form Nxdd,
  ; where N is the variable name, x is either v or d, and dd is a
  ; two-character id.  In that case, the type T of the variable is found by
  ; inspecting the type attribute of N, and (typed! T s ...) is returned.
  ;
  ; Expressions which are of the form (f x ...) represent uninterpreted
  ; functions, whose names are undecorated.  Such functions are typed
  ; against the undecorated name.
  
    (prog (ftype)
	(setq ftype (vartype f))		; get type of object
    (cond ((null ftype) (return (normalize2 f))) ; no type, just normalize
	  (t	 (return (cons 'typed!		; typed, add type function
				     (cons ftype
					   (normalize2 f))))))))
(defun normalize2 (f) 
  ; This function inputs a list of unnormalized forms, and outputs a normalized
  ; list.
       (cond ((atom f) f) 
	     (t (cons (normalize1 (car f)) (normalize2 (cdr f))))))

; these normalizing functions have been added to convert
; the jcode syntax (true!) (false!) and (consti! n) to the
; internal syntax used by the theorem prover.

(defun normtrue (f) 'true)

(defun normfalse (f) 'false)

(defun normconsti (f) (cadr f))

; Integer unary negation is turned into an integer subtraction with
; one argument(?!)
;
(defun normnegi (f) (cons 'subi! (normalize1 (cdr f))))

; When we normalize a record select and store, we put a dollar sign
; in the field name so that it cannot collide with a variable, eliminating
; the problem of ``F = G, therefore R.F = R.G''.
;
; Really there are two 'acceptable' versions of each operator:
;
;    (selectr! r type field)  and (selectr! r type$field)
;    (storer! r type field newval) and (storer! r type field newval)
;
; The normalizing functions convert the first forms to the second forms,
; but leave the second forms essentially untouched.
;
; Note: these functions need type info and shouldn't use 'cons'

(defun normselectr (f)
  (cond ((eq (length f) 4)
	 
	 ; f is of the form (selectr! rec type field)
	 (progvn (rec type field)
		 (match f (* rec type field))
		 (setq rec (normalize1 rec))
		 (list 'selectr! rec (concat type '$ field))))
	
	; This case is used to renormalize selectors (it is really obsolete)
	(t (progvn (rec field)
		   (match f (* rec field))
		   (list 'selectr! (normalize1 rec) field)))))

(defun findfield (f l)
  ; l is a list of the form field[1] type[1] field[2] type[2] ...
  ; Search f to find i such that field[i] = f, and return type[i].
  ; If there is no such i, fail.

  (progvn (field type rest)
	  (or (match l ((field type . *) . rest)) 
	      (break 
	  "-- INTERNAL ERROR in prover - cannot find record field name"))
	  (cond ((eq field f) type)
		(t (findfield f rest)))))

(defun normstorer (f)
  (cond ((eq (length f) 5)			; long form
	 (progvn (rec type field newval type1 rec-type)
		 (match f (* rec type field newval))
		 (setq rec (normalize1 rec))
		 (setq newval (normalize1 newval))
		 (list 'storer! rec (concat type '$ field) newval)))
	(t (list 'storer!			; short form (unchanged)
		 (normalize1 (cadr f))
		 (caddr f)
		 (normalize1 (cadddr f))))))


(defun normassign (f)
  ; f is of the form (assign! (select (select (...(a)))) newval)
  ;
  ; We return an expression that evaluates to the result of
  ; replacing the indicated component of a by newval.
  (normassign1 (cadr f) (caddr f)))

(defun normassign1 (part newval)
  ; part is a compenent of the array a, indicated by an
  ; expression of the form (select (select (.. (a)))).
  ; Return an expression that indicates what happens to a
  ; when part is replaced by newval.
  (progvn (part1 sel)
	  (cond ((match part ('selecta! part1 sel))
		 (normassign1 part1 (list 'storea! part1 sel newval)))

		((match part ('selectr! part1 sel))
		 (normassign1 part1 (list 'storer! part1 sel newval)))

		(t (normalize1 newval)))))
;
; boolean normalizing functions
;
;	normequal  --  normalize equality operator
;
;	We distinguish between equality between booleans and other
;	types of equality. 
;
(defun normequal (f)
  (prog (a b)
	(setq a (normalize1 (cadr f)))
	(setq b (normalize1 (caddr f)))
	(return (cond ((and (isboolean a) (isboolean b))
		       (booleanequality a b)) ; force casing
		      (t (cons boolsymeq
				   (cons a (cons b nil))))))))
	       
(defun isboolean (f)
  ; return a boolean indicating whether or not f is boolean.
  ; assumptions: f is normalized; all conditionals are booleans.
  ; For variables and fields, we look at the type information.
  (prog (type tag)
	(return (cond ((atom f) (memq f '(true false))) ; if true or false, OK
		      ((memq (car f) predicatenames) t) ; if predicate, OK
		      ((match f ('typed! (type . *) . *)) ; if typed and bool
		       (eq type 'boolean))	; if typed, check
		      ((memq (car f) '(selectr! storer! selecta! storea!))
		       (eq (car (selectortype f)) 'boolean))
		      (t nil)))))			; otherwise nil
;
;	booleanequality  --  represent equality of two truth values
;
;	It is not sufficient, when we want to assert strong equality
;	between A and B as Boolean-valued objects, to say A=B.
;	We need to generate
;	     (A and B) or ((not A) and (not B))
;	to force case analysis.  For terms present in the original formula,
;	this is done in normalizeequal.  For formulae generated in daemons,
;	this task must be performed explicitly.
;
(defun booleanequality (a b)
	(cond ((eq a 'true) b)			; optimize a=true case
	      ((eq b 'true) a)			; optimize b=true case
	      (t (list boolsymor		; default - must do casing
		(list boolsymand a b)
		(list boolsymand (list boolsymnot a) (list boolsymnot b))
		))))
;
;	selectortype  --  get type of selector-valued expression
;
;	Used only by isboolean
;
(defun selectortype (f)
	(cond ((atom f) (internalerror "Null selector expression"))
	      ((eq (car f) 'typed!) (cadr f))		; if typed, return
	      ((eq (car f) 'storer!) (selectortype (cadr f)))
	      ((eq (car f) 'storea!) (selectortype (cadr f)))
	      ((eq (car f) 'selecta!) 			; (selecta! array ix)
		(caddr (selectortype (cadr f)))); get from defn
	      ((eq (car f) 'selectr!)
		(cadr (assoc (caddr f) (cddr (selectortype (cadr f))))))
	      (t (internalerror "Bad selector expression"))))
;
;	normnotimplies  --  turn notimplies operator into not of implies
;
(defun normnotimplies (f)
       (list boolsymnot (normalize1 (list boolsymimplies (cadr f) (caddr f)))))

;
;	normimpliedby  --  turn impliedby operator around and into implies
;
(defun normimpliedby (f)
       (normalize1 (list boolsymimplies (caddr f) (cadr f))))

;
;	normnotimpliedby  --  do both
;
(defun normnotimpliedby (f)
       (normalize1 (list boolsymand (caddr f) (list boolsymnot (cadr f)))))

;
;	normnotequal  --  handle not equal as not of equals
;
(defun normnotequal (f)
       (list boolsymnot (normalize1 (list boolsymeq 
                                   (cadr f) 
                                   (caddr f))))) 
;
;	definejsyntax  --  define Jcode built-in names
;
;	Called once during initialization
;
;	The normalizing function for each operator requiring normalization
;	is associated with the operator.
;
(defun definejsyntax () 
  (mapc 'putprop
     '(true!      false!     consti!      negi!       selectr!     storer!)
     '(normtrue   normfalse  normconsti   normnegi    normselectr  normstorer)
     '(normalize  normalize  normalize    normalize   normalize    normalize))

  (mapc 'putprop
     '(assign!)
     '(normassign)
     '(normalize))

  (mapc 'putprop
     '(equal!	 notequal!	notimplies!	impliedby!    notimpliedby!)
     '(normequal normnotequal   normnotimplies  normimpliedby normnotimpliedby)
     '(normalize normalize      normalize	normalize     normalize)))
