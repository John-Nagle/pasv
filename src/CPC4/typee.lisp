;
;	Interface between the Egraph and the theory of types
;
;					Version 1.40 of 2/3/86
;					John Nagle
;
(declarespecial
		voidtype			; the void type constant
		booleantype			; boolean type constant
		integertype			; integer type constant
		universaltype			; universal type constant
		optyper				; property for typing fn
		zsymge				; >= operator
		truenode			; true enode
		falsenode			; false enode
		rootnode			; for signaltypechange
		)
;;;(declare
;;;  (load 'need.o) (load 'defmac.o) (load 'hunkshell.o) (load 'enode.o))
;
;	atomdatatype  --  data type of atom
;
(defun atomdatatype (f)
	(cond 	((get f 'optype))		; get type if available
		(t universaltype)))		; default is universal
;
;	commontype  --  takes two types and returns the type common
;			to both.  If the types are totally disjoint,
;			(void) is returned.  Types are identical to those
;			used in Jcode.
;
(defun commontype (t1 t2)
	(cond	((equal t1 t2) t1)		; if same, return type
		((null t1) (internalerror "null type t1 in commontype"))
		((null t2) (internalerror "null type t2 in commontype"))
		((atomp t1) (internalerror "atom type t1 in commontype"))
		((atomp t2) (internalerror "atom type t2 in commontype"))
		((eq (car t1) (car voidtype)) voidtype)	; void matches nothing
		((eq (car t2) (car voidtype)) voidtype)	; void matches nothing
		((eq (car t1) 'universal) t2)	; universal matches anything
		((eq (car t2) 'universal) t1)	; universal matches anything
						; integer matches subrange
		((and (eq (car t1) 'integer) (eq (car t2) 'subrange)) t2)
		((and (eq (car t2) 'integer) (eq (car t1) 'subrange)) t1)
		((not (eq (car t1) (car t2))) voidtype) ; fails if disjoint
		((eq (car t1) 'subrange) (commonsubrange t1 t2)) ; subrange case
		((eq (car t1) 'array) (commonarray t1 t2)) ; array special case
		((eq (car t1) 'record) (commonrecord t1 t2)) ; record case
		(t voidtype)))			; default - no commonality
;
;	commonarray  --  takes two array types and returns the type common
;			 to both by recursive analysis.
;
(defun commonarray (t1 t2)
    (prog (subscripttype elementtype)		; working vars
	(setq subscripttype (commontype (cadr t1) (cadr t2))) ; type of subscr
	(setq elementtype (commontype (caddr t1) (caddr t2))) ; type of elt
	(cond	((eq (car voidtype) (car subscripttype)) 
			(return voidtype)) ; subscript disjoint
		((eq (car voidtype) (car elementtype)) 
			(return voidtype)))	; element type
	(return (list				; return new array item
		    'array			; (array (subscrtype) (elttype))
		    subscripttype		; type of subscript
		    elementtype))))		; type of element
;
;	commonsubrange  --  find common subrange of two subranges
;
;	Returns voidtype if disjoint.
;
(defun commonsubrange (t1 t2)
    (prog (low high)				; final bounds
	(setq low (max (cadr t1) (cadr t2)))	; new low bound
	(setq high (min (caddr t1) (caddr t2)))	; new high bound
	(cond ((lessp high low) (return voidtype))) ; voidtype if disjoint
	(return (list 'subrange low high)))) 	; return new subrange item
;
;	commonrecord  --  find common record of two records
;			  Only identical records match.
;
(defun commonrecord (t1 t2)
    (cond ((equal t1 t2) t1)			; if same, return type
	  (t voidtype)))			; otherwise voidtype
;
;	initialization of type constants
;
(setq voidtype '(void))				; the void type
(setq booleantype '(boolean))			; the boolean type
(setq integertype '(integer))			; the integer type
(setq universaltype '(universal))		; the universal type
;
;	Functions for setting and getting types from nodes
;
;
;	getdatatype  --  get data type of node
;
;	The data type is taken from the root node of the equivalence class
;
(defun getdatatype (node)
	(cond ((not (isenode node)) 
		(internalerror "getdatatype of non-Enode")) ; must be enode
	      ((null (edatatype (eroot node)))
		(internalerror "getdatatype = nil")))
	(edatatype (eroot node)))		; return data type of root
;
;	setdatatype  --  change data type of node
;
;	The old type is saved on the context stack, so
;	popcontext can undo this transaction.
;
(defun setdatatype (node newtype)
    (prog (root oldtype)
	(cond ((not (isenode node)) 
		(internalerror "setdatatype of non-Enode"))) ; must be enode
	(setq root (eroot node))		; get root node
	(setq oldtype (edatatype root))		; get old type
	(cond ((equal oldtype newtype) (return))) ; if no change, return
	(changedatatype root oldtype newtype)	; change the type
	(and (equal (car newtype) 'subrange)	; if subrange
	        (setbounds (eroot node) oldtype)) ; if changed, check bounds
	))
;
;	zsetdatatype  --  change data type of node, no Z update.
;			  Used only from Z.
;
(defun zsetdatatype (node newtype)	
    (prog (root oldtype)
	(cond ((not (isenode node)) 
		(internalerror "setdatatype of non-Enode"))) ; must be enode
	(setq root (eroot node))		; get root node
	(setq oldtype (edatatype root))		; get old type
	(changedatatype root oldtype newtype)	; change the type
	))
;
;	changedatatype  --  store into data type field
;			    A transaction is queued for pushcontext, and
;			    the typewait queue is checked.
;
(defun changedatatype (root oldtype newtype)
	(xedatatype root newtype)		; change the type
	(pushcontext (cons 'popdatatype (cons root oldtype))) ; save for pop
	(signaltypechange root)			; process typewaits
	)
;
;	popdatatype  --  undo setdatatype
;
;	popdatatype is called during context restoration to undo one
;	setdatatype operation.  The list cell given contains
;	(node . oldtype)
;
(defun popdatatype (item)
	(xedatatype (car item) (cdr item)))	; restore old value
;
;	Type wait queue management
;
;	The type wait queue is a list of transactions associated with a
;	node.  When the type of a node is changed, the transactions 
;	on the queue are processed.  Processing a transaction uses it up.
;
;	Transactions are of the form
;
;	(function arg2 arg3 arg4  ... )
;
;	and a transaction is processed by calling the function with the
;	arguments
;	    root arg2 arg3 arg4 ...
;
;	Note that the root node of the class may change through emerges.
;
;	It should be understood that this mechanism is not used very often.
;	Its primary purpose is to allow the refiring of demons and the
;	restarting of subprovers when a node of universal type is seen and
;	it is necessary to wait for a type to appear.  In practice, this
;	happens only when rules are instantianted.
;
;
;	emergetwait  --  merge type wait lists
;
;	This is used by emerge.
;
(defun emergetwait (root queue1 queue2)
    (prog (queueold)
	(setq queueold (etypewait root))	; get old queue
	(and (null queueold) (null queue1) (null queue2) ; all-nil case
	     (return))				; no action required
    	(xetypewait root (append queue1 queue2)); construct new queue
						; queue undo transaction
	(pushcontext (cons 'poptypewait (cons root queueold))) 
	))
;
;	poptypewait  --  undo emergetwait transaction (from popcontext)
;
;	Queued item has form ( node . oldvalue )
;
(defun poptypewait (item)
    (xetypewait (car item) (cdr item)))			; undo
;
;	queuetypewait  --  queue new typewait transaction
;
;	Duplicate transactions are ignored.
;
(defun queuetypewait (node fn arglist)
    (prog (root wait)
	(setq root (eroot node))		; get root node
	(setq wait (cons fn arglist))		; new wait term
	(cond ((member wait (etypewait root)) 	; if duplicate
		(rareevent "Duplicate typewait"); log
		(return)))			; and ignore
						; save old state for pop
	(pushcontext (cons 'poptypewait (cons root (etypewait root))))
						; new entry at head
	(xetypewait root (cons wait (etypewait root)))
	(return nil)))				; return nil
;
;	signaltypechange  --  type change detected, do all transactions
;
(defun signaltypechange (rootnode)
    	(cond ((etypewait rootnode)		; if any work to do
	       (prog (worklist)			; begin block
		     (setq worklist (etypewait rootnode))	; get work list
		     (pushcontext 		; save for undo
			(cons 'poptypewait (cons rootnode worklist)))
		     (xetypewait rootnode nil)	; clear work list
		     (mapcar 'signaltypechange1 worklist) ; do all trans
		))))
;
;	signaltypechange1  --  handle single type change transaction
;
(defun signaltypechange1 (trans)
	(rareevent "Type wait completion")	; tally these events
	(apply (car trans) (cdr trans))) 	; do it
;
;	Demon type wait routines
;
;	dtwait  --  demon type wait
;
;	Called at the beginning of demons.  If any matched
;	term is of type "universal", try the demon again later
;	and return t now.  Usually invoked by
;
;	(and (dtwait 'demon node pmatchlist lab pattern) (return))
;
;	so that the demon exits immediately if true is returned.
;
(defun dtwait (demonname node pmatchlist lab pattern)
    (prog (utype)
	(setq utype (finduniversal pmatchlist))	; find universal type if any
	(and (null utype) (return nil))		; if fail, good
	(rareevent (concat "Type wait for " demonname)) ; log wait
	(queuetypewait node demonname (list node pmatchlist lab pattern))
	(return t)))				; tell caller to quit
;
;	finduniversal  --  look for node of universal type in match list
;
(defun finduniversal (pmatchlist)
    (cond ((null pmatchlist) nil)		; if null, done
	  ((equal (getdatatype (cdar pmatchlist)) universaltype)
		(cdar pmatchlist))		; if universal, return node
	  (t (finduniversal (cdr pmatchlist))))); return
;
;	integerptype  --   is type integerp?
;
(defun integerptype (n)
					; true if integer or subrange
    (memq (car (getdatatype n)) '(integer subrange))) 
;
;	emerget  --  merge types of two nodes
;
;	Emerget is called from emerge when the merging of two nodes
;	has just been completed.  Emerget must change the type of the new
;	root node to be the common type of the types of the nodes being
;	merged.  
;	
;	If the types are incompatible (commontype returns nil)
;	this indicates that an inconsistency has been detected and
;	propagatefalse is called to abort analysis of the current 
;	propositional case.  This is an error if the form was not
;	introduced by a rule, in which case we print an error message.
;
(defun emerget (root type1 type2)		; return t if changed
    (prog (newtype)
	(and (equal type1 type2) (return nil))	; skip if types identical
	(setq newtype (commontype type1 type2))	; compute type intersection
	(setdatatype root newtype)		; set new type of root
	(cond ((equal newtype voidtype)		; if new type is void
		(cond ((< (geteheight root) 1)	; err if not from a rule
			(patom "Type clash detected: ")	; ***TEMP DEBUG***
			(patom type1)			; ***TEMP DEBUG***
			(patom " vs ")			; ***TEMP DEBUG***
			(patom type2)			; ***TEMP DEBUG***
			(terpri)			; ***TEMP DEBUG*** 
			(rareevent "Merge type clash"))	; note unusual event
		    (t nil))			; clash from a rule
		(propagatefalse)		; contradiction detected
		(return t))			; no further analysis
		)
	(mapcar 'emerget1 (epredecessors root))	; check parents
	(return t)				; returns t if changed
	))
;
;	emerget1  --  check predecessors of changed node for derived type
;
;	If a node represents a function call whose type is derived from
;	the type of its successors (such as "selecta") there will be
;	an "optyper" property for the function atom.  In such a case
;	the typing function must be reevaluated, which may cause further
;	type merging.
;
;
(defun emerget1 (node)
    (prog (fn fntyper newtype)
	(setq fn (getfnatom node))		; get function atom  
	(and (null fn) (return))		; if null, return
	(setq fntyper (get fn 'optyper))	; get typing routine
	(and (null fntyper) (return))		; if null, return
	(and (atomp (esuccessors node)) (return)); if successors are not list
	(setq newtype (funcall fntyper (esuccessors node))); derive new type
	;	Merge new derived type if required
	(cond ((not (equal newtype 		; if type changed
			   (getdatatype node)))
	     	(rareevent "emerget1 changed type"); tally to find out
	     	(emerget node (getdatatype node) newtype)))
	))
;
;	getfnatom  --  get function atom for enode, or nil if non-fn
;
(defun getfnatom (fn)
    (cond
	((null fn) nil)				; if null, not fn
	((isenode fn) (getfnatom (esuccessors fn))) ; if enode, 1st successor (CL: check for enode before atom)
	((atomp fn) fn)				; if atom, this is it
	(t (getfnatom (car fn)))))		; if list, 1st elt.
;
;	Bounds routines
;
;	Bounds are of the form (lo . hi)
;	and either or both may be nil.
;
;	getbounds  --  get numeric bounds given type
;
(defun getbounds (type)
    (cons
	(cond ( (null type) nil)		; if null type, nil
		((equal (car type) 'subrange) (cadr type)) ; if subrange, bound
	      	(t nil))			; otherwise nil
	(cond ( (null type) nil)
		((equal (car type) 'subrange) (caddr type))
	      	(t nil))))
;
;	setbounds  --  tell Z prover about new numeric bounds of node
;
;	Whenever the bounds of a subrange change, setbounds updates
;	the information available to the Z prover, by generating 
;	terms (node <= highbound) and (node >= lowbound) as required.
;	If lowbound = highbound, (node = highbound) is generated.
;
;	Either bound may be nil, indicating no bound in that direction.
;
;	The old type is provided as an optimization; if the old type
;	has a bound the same as that of the new type, the Z prover
;	need not be provided new information.
;
(defun setbounds (fnode oldtype)
    (prog (newbounds oldbounds)
	(setq newbounds (getbounds (edatatype fnode))) ; get new bounds
	(setq oldbounds (getbounds oldtype))	; get old bounds
	(cond ((and (cdr newbounds) (car newbounds)) ; if both non-nil
	       (cond ((lessp (cdr newbounds) (car newbounds)) ; hi < lo?
		    	(internalerror "Subrange bounds cross"))
		     ((equal (cdr newbounds) (car newbounds)) ; hi = lo?
			(emerge fnode (enode (cdr newbounds))) ; node = hi
			(return)		; done 
			))))
    	(cond ((and (cdr newbounds) 		; if new high bound
	            (not (equal (cdr newbounds) (cdr oldbounds))))
	       (emerge truenode 		; Assert hi >= fnode
		    (enode (list zsymge (cdr newbounds) fnode)))))

    	(cond ((and (car newbounds)		; if new low bound
	      	    (not (equal (car newbounds) (car oldbounds))))
	       (emerge truenode 		; Assert fnode >= lo
		    (enode (list zsymge fnode (car newbounds))))))
    ))
;
;	typefrombounds  --  construct type expr given bounds
;
;	Input is of the form (low . hi)
;
;	Returns (subrange lo hi) if both bounds are non-nil, otherwise
;	returns (integer)
;
(defun typefrombounds (bounds)
    (cond ((and (car bounds) (cdr bounds))
		(list 'subrange (car bounds) (cdr bounds)))
	   (t integertype)))
;	
;	typenonatomic  --  type a new nonatomic enode
;
;	Called from enodenonatomic as a new node is being constructed.
;	The argument is a list of nodes representing the arguments
;	to a function, builtin or otherwise.
;	Functions such as "addi!", etc. have canned constant types
;	associated with the property 'optype.  Generic functions
;	such as "selecta!" have typing routines associated with the
;	property 'optyper.  Note that when a generic typing routine
;	is called, it is given a list of enodes (including the fn
;	node) which have each already been typed individuallly.
;	Thus, a generic typing routine is able to obtain the types
;	of the arguments to which a function is being applied.
;
(defun typenonatomic (l)
    (prog (fn typer)
	(setq fn (esuccessors (car l)))		; get fn atom
	(return (cond				; return type of expression
	    ((get fn 'optype))			; type of op if present
	    ((setq typer (get fn 'optyper))	; typing routine if present
		(funcall typer l))		; use typing routine for fn
	    (t universaltype)))))		; default is universal
;
;	typedenode  --  use information from "(typed! <type> f arg1 ...)
;
; 	f is assumed to be of the form (typed! t x ...), where t is a type.
; 	This function returns an enode representing
; 	(x ...), and as a side effect asserts that (x ...) is of type (K . t). 
;
;	If the node already has a type, the types are merged.
;	This is necessary because the "typed!" predicate may be rescanned
;	many times during case analysis.  The actual merging is done
;	in enodenonatomic.
;
(defun typedenode (f) 
  (prog (fnode newtype) 			; the enode returned for f
	(setq newtype (cadr f))			; get type from "typed!"
	(setq fnode (enodenonatomic (cddr f) t newtype)) ; make node
	(return fnode)))
;
;	Typing routines for specific operators
;
;
;	Typing routines may return the universal type when the type of
;	the operands is universal.  If the type of an operand changes
;	later, the typing routine will be called again by emerget to
;	update the derived type.
;
;	typeselecta  --  typing routine for selecta.
;
;	We presently detect an internal error if selecta! is applied
;	to a non-array non-universal object.  This should be handled better.
;
(defun typeselecta (l)				; given fn call enode list
    (prog (argtype)				; given array term
	(setq argtype (getdatatype (cadr l)))	; get data type of arg 1
	(return (cond				; return based on case
	    ((equal argtype universaltype) universaltype) ; universal maintains
	    ((eq (car argtype) 'array) (caddr argtype)) ; elt type if array
	    (t (internalerror "selecta! of non-array")))))); USER CAN CAUSE THIS
;
;	typestorea  --  typing routine for storea.
;
(defun typestorea (l)
    (prog (argtype)				; given array term
	(setq argtype (getdatatype (cadr l)))	; get data type of arg 1
	(return (cond				; return based on case
	    ((equal argtype universaltype) universaltype) ; universal maintains
	    ((eq (car argtype) 'array) argtype) ; same type if array
	    (t (internalerror "storea! of non-array")))))); USER CAN CAUSE THIS
;
;	typeselectr  --  typing routine for selectr
;
;	The type of a selectr! is that of the field selected.
;
(defun typeselectr(l)
    (prog (argtype fieldname resulttype)
	(setq argtype (getdatatype (cadr l)))	; get data type of arg 1
	(setq fieldname (esuccessors (caddr l))); get field name being selected
	(return (cond
	    ((equal argtype universaltype) universaltype) ; universal unchanged
	    ((eq (car argtype) 'record)		; if record
		; record - look up field name in type definition
		(setq resulttype (assoc fieldname (cddr argtype)))
		(and (null resulttype)		; if not found
		     (internalerror 		; USER-CAUSABLE ERROR
			(concat "Record field in selectr! not found: "
			    (implode (explode fieldname)))))
		(cadr resulttype))		; return result type
	    (t (internalerror "selectr! of non-record"))))))
;
;	typestorer  --  typing routine for storer
;
;	The type of a storer! is that of the first argument.
;
(defun typestorer(l)
    (prog (argtype)
	(setq argtype (getdatatype (cadr l)))	; get data type of arg 1
	(return (cond
	    ((equal argtype universaltype) universaltype) ; universal unchanged
	    ((eq (car argtype) 'record) argtype)	; same type as arg
	    (t (internalerror "storer! of non-record"))))))
;
;	Initialization for typing routines
;
;	The relevant function symbols are tagged with the appropriate routine
;
(putprop 'selecta! 'typeselecta 'optyper)	; set typing routine
(putprop 'storea! 'typestorea 'optyper)		; set typing routine
(putprop 'selectr! 'typeselectr 'optyper)	; set typing routine
(putprop 'storer! 'typestorer 'optyper)		; set typing routine
;
;	Operator result type initialization for operators of constant type
;
(defun setoptype (typ lst)			; set all on lst to typ
    (cond (lst
	(putprop (car lst) typ 'optype)		; set type of operator
	(setoptype typ (cdr lst)))))		; recurse

(setoptype booleantype '(
	true
	false
	alltrue!
	and!
	arrayp!
	arraytrue!
	booleanp!
	equal!
	gei!
	gti!
	gtn!
	impliedby!
	implies!
	integerp!
	lei!
	lti!
	not!
	notequal!
	notimpliedby!
	notimplies!
	numberp!
	or!
	zerop!
	))
(setoptype integertype '(
	addi!
	addn!
	divi! 
	divide!
	divn!
	mod!
	modi!
	muli!
	muln!
	negi!
	subi!
	subn!
	))
