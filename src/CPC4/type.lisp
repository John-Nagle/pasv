;
;	Type machinery		Version 1.8 of 12/13/82
;
(declarespecial
	getftype
	begin-decl
	end-decl
	popdecl
	errport
	recname
	seriouserrors
	)
;
;	getdtype  --  get D-type of object
;
;	The D-type of an object is the type of its definedness part.
;	This is a structure with booleans in place of all the variable parts.
;	
(defun getdtype (var-type)
	(cond	((atom var-type) (internalerror "Bad type declaration"))
		((eq (car var-type) 'array)	; if array
		    (list (car var-type)	; array
			(cadr var-type) 	; subscript type
			(getdtype (caddr var-type)))) ; data type
		((eq (car var-type) 'record)	; if record
		    (append
		      (list (car var-type)	; 'record
			  (cadr var-type))	; type name
		      (mapcar 'getftype (cddr var-type))))
		(t '(boolean))))		; if simple type
;
;	getftype --  get D-type of record field expression
;
;	A field expression is a two element list of field name and type.
;
(defun getftype (fieldexpr)
	(list (car fieldexpr) (getdtype (cadr fieldexpr))))
;
;	vartype  --  return type of variable or function name, given
;		     only the name.
;
;	Names are decorated by adding to the end of the name as follows:
;
;	xxxx!		built-in function, no type
;	xxxxv01		variable, value reference
;	xxxxd01		variable, definedness reference
;	xxxx		user function (no decoration)
;
(defun vartype (varname)
     (prog (xname xletters xtype plainname vdflag)
	(cond ((atom varname) (return nil)))	; does not apply to atom
	(setq xname (car varname))		; get fn atom or var atom
	(cond ((not (atom xname)) (return nil))); not atom, inapplicable
	(setq xletters (nreverse (explode xname))); get letters of name
	(cond ((eq (car xletters) '!) (return nil)) ; built in fn
	      ((null (cdr varname))		; if variable, not fn call
		; Simple variable case.
		; Name NEW count must not be 00 - indicates no NEW in VCG
		(and (eq (car xletters)  '|0|)
		     (eq (cadr xletters) '|0|)
		     (internalerror (concat (implode (explode varname)) 
				" ends in 00")))
		;	Look up undecorated part of decorated name
		(setq vdflag (caddr xletters))	; v or d
						; basename
		(setq plainname (implode (nreverse (cdddr xletters))))
		)
	       (t 
		;	Function case
		(setq vdflag 'v)		; Always value part of fns
		(setq plainname xname)		; fn names are undecorated
		))
	(setq xtype (get plainname		; now get the type
	    (cond ((eq vdflag 'v) 'vtype)	; if v, get value type
		  ((eq vdflag 'd) 'dtype)	; if d, get def type
		  (t nil))))			; otherwise fails
	(and (null xtype) 
	    (internalerror (concat "Undeclared variable: " 
		(implode (explode varname)))))
	(return xtype)))			; return result
;
;	begin-decl  --  begin a J-unit of declarations
;
(defun begin-decl ()
  (pushcontext 'decl))
;
;	end-decl  --  end a J-unit of declarations
;
; 	This function is called to remove all declarations inserted since
;	the last call to begin-decl.
;
(defun end-decl ()
  (popcontext 'decl))
;
;	popdecl  --  called by popcontext to remove a declaration from
;		     a property list.
;
(defunobj popdecl (x)
  (remprop x 'dtype)
  (remprop x 'vtype))
;
;	vardecl  --  declare a variable
;
;	Called by direct input from the VC pass.
;
;	Form is 
;	   (vardecl <name> (<class> <type>))
;	where <class> and <type> are as described in the Jcode document.
;	
(defun vardecl (var-name var-info)
  (prog (var-type var-kind)
	(setq var-type (cadr var-info))		; get type
	(setq var-kind (car var-info))		; function, variable, etc.
	(checkvardecl var-name var-kind var-type) ; check for illegality
        (putprop var-name var-type 'vtype)	; type of V part
        (putprop var-name (getdtype var-type) 'dtype) ; type of D part
        (pushcontext (cons popdecl var-name))))
;
;	checkvardecl  --  check type consistency for rules
;
;	Type consistency is not a problem for variables, since
;	variables do not appear by name in rules.
;	For functions other than rule functions, the function
;	may be declared in the rule builder only with a DCL,
;	i.e. as an uninterpreted function.  For rule functions,
;	the type in the program and in the rule database must
;	match exactly. 
;
(defun checkvardecl (name kind type)
    (cond ((get name 'vtype) 			; check for redefinition
	    (internalerror (concat "Variable multiply defined: " name)))
	  ((eq kind 'variable))			; if variable, skip
	  ((eq kind 'function)			; program function
	     (cond ((get name 'defn)		; if DEFN, bad
		(patom '|RULE ERROR - "| errport)
		(patom name errport)
		(patom 
	  	'|" is both a non-rule function and a rule DEFN.|
		errport)
		(terpri errport)
		(setq seriouserrors (add1 seriouserrors)) ; count errors
		)))
	  ((eq kind 'rulefunction)		; if rule function
	      (cond ((null (get name 'defn))) ; if not DEFN, OK
		    ((not (equal type (get name 'defntype)))
			(patom '|RULE ERROR - "| errport)
			(patom name errport)
			(patom '|" is | errport)
			(patom type errport)
			(patom " in the program but " errport)
			(patom (cond ((get name 'defntype))	
	       		('(unknown)))
	       			errport)	
			(patom " in the rules." errport)
			(terpri errport)
			(terpri errport)
			(setq seriouserrors (add1 seriouserrors)) ; count errors
			)))
	  (t (internalerror "Unknown kind of variable")))
	)
