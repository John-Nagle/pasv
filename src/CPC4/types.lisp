;
;	Type machinery
;
(declare (special
	getftype
	begin-decl
	end-decl
	popdecl
	errport
	))
;
;	getdtype  --  get D-type of object
;
;	The D-type of an object is the type of its definedness part.
;	This is a structure with booleans in place of all the variable parts.
;	
(defun getdtype (var-type)
	(cond	((atom var-type) (break "INTERNAL ERROR - bad type decl"))
		((eqstring (car var-type) 'array)	; if array
		    (list (car var-type)	; array
			(cadr var-type) 	; subscript type
			(getdtype (caddr var-type)))) ; data type
		((eqstring (car var-type) 'record)	; if record
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
	(setq xletters (reverse (explode xname))); get letters of name
	(cond ((eqstring (car xletters) '!) (return nil)) ; built in fn
	      ((null (cdr varname))		; if variable, not fn call
		; Simple variable case.
		; Name NEW count must not be 00 - indicates no NEW in VCG
		(and (eqstring (car xletters)  '|0|)
		     (eqstring (cadr xletters) '|0|)
		     (break "-- INTERNAL ERROR in prover - name ends in 00"))
		;	Look up undecorated part of decorated name
		(setq vdflag (caddr xletters))	; v or d
						; basename
		(setq plainname (implode (reverse (cdddr xletters))))
		)
	       (t 
		;	Function case
		(setq vdflag 'v)		; Always value part of fns
		(setq plainname xname)		; fn names are undecorated
		))
	(setq xtype (get plainname		; now get the type
	    (cond ((eqstring vdflag 'v) 'vtype)	; if v, get value type
		  ((eqstring vdflag 'd) 'dtype)	; if d, get def type
		  (t nil))))			; otherwise fails
	(and (null xtype) 
	    (break
		"-- INTERNAL ERROR in prover - undeclared variable"))
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
(defun vardecl (var-name var-type)
  ; This function is called to declare a variable that appears in VCs.
  ; We trust the caller not to declare a variable more than once in the
  ; same j-unit.
  ; We check, though, that the user does not define the same function
  ; as an interpreted function in a rule and in the program.
  (cond ((and (get var-name 'defn) (not (equal  var-type '(module))))
	(patom '|FATAL ERROR - "| errport)
	(patom var-name errport)
	(patom 
	  '|" is both a non-rule function and a rule DEFN.|
		errport)
	(terpri errport)
	(exit 1)))
  (putprop var-name var-type 'vtype)	; type of V part
  (putprop var-name (getdtype var-type) 'dtype) ; type of D part
  (pushcontext (cons popdecl var-name)))
