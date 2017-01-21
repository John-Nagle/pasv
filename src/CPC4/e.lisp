"@(#)e.l	2.41"

;;;(declare
;;;  (load 'need.o) (load 'defmac.o) (load 'hunkshell.o) (load 'enode.o)
;;; (load 'debug.o) (load 'princ.o) (load 'map.o))
  
(needs-macros)

(declarespecial
   boolsymeq
   contextstack
   demonavailist
   demonnumber        ; statistic
   eavailist          ; free enode that was allocated the earliest
   eduplicatenumber
   efirednumber       ; statistic
   efiringlist
   eformlist
   eheightmax
   emergenumber
   enodelist
   enumber            ; the sequence number that will be assigned to the
		      ; next enode that is allocated
   enumlist
   epvars
   estats
   etime
   falsenode
   freevariables
   functionnames
   heightover
   ifexistsnumber     ; statistic
   ifthennumber       ; statistic
   makemergenumber    ; statistic
   pheightmax         ; limit
   predicatenames
   pmatchlist
   quotednodelist     ; a list of dotted pairs of the form (f . n), where f
		      ; is an enode representing a function, and n is an 
		      ; integer.  The pair indicates that the n'th argument of
		      ; f is implicitly quoted.  Only one postition of any
		      ; function can be implicitly quoted.
   ruledepth
   rulelist
   rulename
   rulenodelist
   ruletag
   ruletally
   instantiatetag
   slowmode
   tab
   truenode
   turnon
   zfunctionnodes
   zgenode
   zgtnode
   zmultnode
   zsymge
   zunsplice)
;
;	enode  --  returns the enode representing f, where f is either
;	an S-expression or an enode.
;
;	Enode does much more than this; it is really the starting point
;	of much of the proof process.  The enode returned has had
;	the theory of equality applied to it; duplicate nodes have
;	been eliminated in accordance with the theory of uninterpreted
;	functions, the appropriate information has been sent to the
;	numeric prover and the demon system, and the type of the node
;	has been recalculated.
;
;	It is necessary to pass through enode whenever the linking
;	structure of enodes (via esuccessors) is changed, so that
;	the type information is reprocessed.  See instantiate2.
;
(defun enode (f) (cond ((numberp f)          (enodenumber f))
                       ((atom f)             (enodeatom f))
                       ((isenode f)          f)
                       ((eq (car f) 'typed!) (typedenode f))
		       (t                    (enodenonatomic f t nil))))

;;; (enodeatom 'f) returns the enode representing f, where f is an atom.
;;; If an atom is represented by an enode, that enode is stored under the
;;; 'enode indicator of the property list of the represented atom.

(defun enodeatom (f) 
       (cond ((get f 'enode))
             (t (prog (node) 
                      (setq node (newenode))
                      (putprop f node 'enode)
                      (xesuccessors node f)
		      (setdatatype node (atomdatatype f)) ; set atom type
                      (return node)))))
             
(defunobj poppredecessors (pair) 
  ; This function is called as the node creation performed in enodenonatomic 
  ; is being undone.  
  (prog (x) 
	(setq x (cdr pair))
	(or (eq (car pair) (car (epredecessors x))) 
		(internalerror "poppredecessors"))
	(xepredecessors x (cdr (epredecessors x))))) 

;;; enodenonatomic is where all the real work is done.  f is assumed to
;;; be a list of S-expressions and/or enodes.  First, a list l is formed
;;; containing the enodes representing each of the elements of f.  Then
;;; an enode whose successors are congruent to the elements of l is either
;;; located or created.  fun is a boolean indicating whether or not the
;;; argument is considered to be a function invocation.
;;; nodetype, if not nil, is to be used as the type of the resulting node.
;;; nodetype is set only by typedenode.

(defun enodenonatomic (f fun nodetype) 
  (prog (node        ; a work variable containing an enode
	 candidates  ; a list to search to find node
	 len         ; the number of elements of f
	 finish      ; the end of a circular list being searched
	 l           ; the enodes representing the elements of f
	 height	     ; depth into rules
	 funatom     ; atom of function name if applicable
	 )

	(setq len (length f))		; count args to f
	(setq l (mapcar 'enode f))	; do all subordinate terms
	(cond ((null nodetype)		; if node type not given
	      (setq nodetype (typenonatomic l)))) ; get type of new node

	; Define the ``extended predecessor list'' of an enode E to be the
	; concatenation of the predecessor lists of all enodes that are
	; equivalent to E (including E itself).  We must find or create an
	; enode whose successors are congruent to the elements of l.
	; If such an enode exists, it must be on the extended predecessor
	; list of every enode in l.  We can therefore find the node we are
	; searching for (if it exists) by searching the extended predecessor
	; list of any element of l.  We use the second element of l if there
        ; is one, because we expect its extended predecessor list to be
	; shorter than that of the first element of l.

	(setq node (cond ((cdr l) (cadr l)) (t (car l)))) 
	(setq finish node)
	a
	(setq candidates (epredecessors node))
	b
	(cond ((null candidates)
	       (setq node (eqclass node))
	       (and (eq node finish) (go c))
	       (go a))
	      
	      ; test if (car candidates) is congruent to l.  A simple
	      ; length comparison will eliminate many candiates; if
	      ; that test if passed, do full congruence checking.
	      ((and (= len (eslength (car candidates)))
		    (congruent l (esuccessors (car candidates))))

	       ; the test succeeded; return the congruent node
	       ; after finding the common type of the new node.
	       ; Note: this may propagate false.
	       (and (emerget (car candidates) 
		    nodetype (getdatatype (car candidates)))
		    (signaltypechange (car candidates))) ; type wait trigger
		(seteheight (car candidates) 	; merge height
		    (min ruledepth (geteheight (car candidates))))
	       (return (car candidates)))

	      ; the test failed; check the next predecessor
	      (t (setq candidates (cdr candidates)) (go b)))

	c
	; Come here if all the the entire extended predecessor list
	; has been searched to no avail.  Allocate a new enode. 
	(setq node (newenode))
	(xesuccessors node l)
	(xeslength node len)
	(setdatatype node nodetype)	; set type of new node

	; eheight is the max of the eheights of the nodes below and
	; the current rule depth.
	(setq height ruledepth)		; start from ruledepth
	(mapcone (setq height (max height (geteheight x))) l)
	(seteheight node height)		; set height of new node

	; Put the new node on predecessor list of each element of l,
	; unless it is already there.  (This can only happen if there
	; are duplicates in l).  Record these changes on the context
	; stack.
	(mapcone (cond ((not (memq node (epredecessors x)))
			(pushcontext (cons poppredecessors (cons node x)))
			(xepredecessors x (cons node (epredecessors x)))))
		 l)

	; if f is a function invocation, then (car f) must either be
	; an atom or an enode representing an atom.  (If neither of these
        ; is true, cause a runtime error.)  Get the 'intern entry
	; of the property list of the atom associated with (car f).  If that
	; entry is non-nil, it is expected to be a function, and it is applied
	; to the newly created node.

	(and fun
	     (and (setq funatom (cond ((atom (car f)) (car f))
			     	((not (isenode (car f))) 
				  (internalerror "enodenon"))
			     	((atom (esuccessors (car f))) 
				  (esuccessors (car f)))
			     	((internalerror "enodenode1"))))
		    (cond ((setq fun (get funatom 'intern))	; get subprover
			   (subprovercall node fun funatom)))   ; use subprover
		    ))

	; pattern?
	(mapeqclass (mapcone (startpattern (car x) (cdr x) node) (epattern x)) 
		    nil (car l))
	(return node))) 
;
;	subprovercall  --  interface for sending information to subprovers
;
;	Called by enodenonatomic when a new function node appears and
;	there is a subprover routine associated with that function.
;	There may also be a type checking routine associated with that
;	function, in which case the node will not be sent to the subprover
;	if the type check fails.  When a type check fails, a transaction
;	is queued so that if the type of the node changes in the future,
;	the type check will be repeated and the subprover notified if
;	the check is successful.
;
;	This mechanism is used to prevent the Z box from working on
;	nonnumeric quantities.
;
(defun subprovercall (node fun funatom)	
  (prog (typechkfn fails)
	(setq typechkfn (get funatom 'interntypechk))	; type check applicable?
	(cond ((null typechkfn)				; if no type checker
	       (funcall fun node)			; just do node
	       (return))				; simple case
	      ((null (setq fails (funcall typechkfn node)))
	       (funcall fun node))			; pass, do node
	      (t
	       (rareevent "Failed subprover type check") ; so note
	       ;   For each failed argument, queue a transaction to try
	       ;   again if the type of the argument changes.
	       (mapcone (queuetypewait x 'subprovercall 
				       (list node fun funatom)) 
			fails)
	       ))))
;
;	geteheight   --  get height from root node of class
;
(defun geteheight (node)
    (eheight (eroot node)))
;
;	seteheight   --  set height in enode, and save on context stack
;
(defun seteheight (node neweheight)
  (cond ((not (equal (eheight node) neweheight)) ; if changing
	 (pushcontext (cons 'popheight (cons node (eheight node)))) ; save
	 (xeheight node neweheight))		; set new height
	))
;
;	popheight  --  undo seteheight
;
(defun popheight (item)
    (xeheight (car item) (cdr item)))		; undo it
	
; Define the datatype ``demon'' (this should probably be moved)
(hunkshell demon demonpattern demonmatchlist demonfather demonthread) 

; This function is called during a popcontext to deallocated an enode
(defunobj popenode (node) 
  ; The idea behind the context stack is to undo all the modifications
  ; made to the data structure in the reverse order that they were made.
  ; Therefore, the node being de-allocated should look exactly like it did
  ; just after it was allocated.  If it does not, we have forgotten to
  ; record a change to the data structure, indicating an error in the
  ; simplifier.
  (and (or (not (= (eqlength node) 1))
	   (not (eq eavailist (eavail node)))
	   (econgruent node)
	   (not (eq node (eroot node)))
	   (not (eq node (eqclass node)))
	   (edemon node)
	   (epattern node)
	   (epredecessors node))
       (internalerror "popenode"))
  
  (and (zfield node) (remnumnode node))
  
  ; If the node represented an atom, remove it from the property list
  ; of that atom.  Then restore the successors and length fields to
  ; the value they had when the node was allocated (nil).  It is necessary
  ; that a node taken off the eavailist look just like a node that is
  ; allocated by alloc-enode and initialized by newenode1, because when
  ; newenode takes a node off eavailist, it does not initialize the node.
  
  (cond ((numberp (esuccessors node)))
	((atom (esuccessors node)) (remprop (esuccessors node) 'enode))
	(t (xeslength node nil)))
  (xesuccessors node nil)
  
  ; All the enode ever allocated are linked together in the order they
  ; were allocated.  evavailist is a pointer to earliest allocated enode
  ; is not currently being used.
  (setq eavailist node)) 

; This function allocates a new enode, creating a new equivalence class
; of size 1.  The existence of the new enode is recorded on the context
; stack.

(defun newenode nil 
       (prog (node) 
             (setq node eavailist)
             (pushcontext (cons popenode node))
             (or (eavail node) (xeavail node (newenode1 (alloc-enode))))
             (setq eavailist (eavail node))
  	     (xeheight node 0)			; set height to 0
             (return node))) 

; This function sets the fields of an enode to be a singleton equivalence
; class, and assigns a sequence number to the node.
(defun newenode1 (node) 
       (xeroot node node)
       (xeqclass node node)
       (xeqlength node 1)
       (xeheight node 0)			; set height to 0
       (xenumber node enumber)
       (setq enumber (1+ enumber))
       node) 

(defunobj poptag (node) (xecongruent node nil)) 

(defun checkcongruent (node1 node2) 
  ; Given two enodes, this function tests them for congruence.
  ; If the two nodes are congruent, they are merged.
  (cond ((or (eq node1 node2)   ; If the two nodes are identical, they
	                        ;    do not need merging.
	     (econgruent node1) ; If either node has its congruent set bit,
	     (econgruent node2) ;    a merge has already been performed.
	     ))
	
	; if the nodes have unequal numbers of successors, they clearly
	; cannot be congruent.
	((not (= (eslength node1) (eslength node2))))
	
	; if all the cheap tests above have failed, we must do full
	; congruence checking.
	((congruent (esuccessors node1) (esuccessors node2))
	 
	 ; if the nodes are congruent then record and perform the merge
	 (pushcontext (cons poptag node1))
	 (xecongruent node1 t)
	 (emerge node1 node2)))) 

; This function tests if the successor lists of two nodes are congruent.
; Normally, two lists are congruent iff their elements are equivalent.
; However, the representation of functions such as recordselect! necessitates
; a stronger definition of congruent.  Example: f=g does not imply
; that (recordselect! x f) equals (recordselect! x g).  The reason is that
; recordselect implicitly quotes its second argument.  The variable
; quotednodelist contains a list of dotted pairs of the form (f. n),
; where f is an enode and n is an integer,  indicating that the n'th
; position of the function represented by f is implicitly quoted. 
; Arguments in implictly quoted positions must match exactly for congrugence
; testing to succeed.  quotednodelist is a bit of a kludge:  only one
; position of any function can be quoted and all arguments to quoted
; positions had better be atoms.
;

(defun congruent (l1 l2) 
; test if the two lists l1 and l2 are congruent
  (and (eq (eroot (car l1)) (eroot (car l2)))
       (cond ((assq (car l1) quotednodelist)
	      (congruent2 (cdr l1)
			  (cdr l2)
			  (cdr (assq (car l1) quotednodelist))))
	     (t (congruent3 (cdr l1) (cdr l2)))))) 

(defun congruent2 (l1 l2 n)
; test if l1 and l2 are equivalent, except for the n'th postition,
; where they must be identical.  l1 and l2 are assumed to have
; the same number of elements.
       (cond ((null l1))
             ((= n 1) (and (eq (car l1) (car l2))
			   (congruent3 (cdr l1) (cdr l2)))) 
             (t (and (eq (eroot (car l1)) (eroot (car l2))) 
                     (congruent2 (cdr l1) (cdr l2) (1- n))))))

(defun congruent3 (l1 l2) 
; test if l1 and l2 are equivalent.  The lists are assumed to have
; the same number of arguments.
       (or (null l1)
           (and (eq (eroot (car l1)) (eroot (car l2)))
		(congruent3 (cdr l1) (cdr l2))))) 


;;;============================================================================
;;; emerge, mergedemons


(defunobj popmerge (root2) 
  ; Undo the merge performed below.
  ; It is important to remember that when popmerge is called, the
  ; situation will be exactly is depicted in the diagram below.
  (prog (root1 start1) 
	; by following links, root1 and start1 can be reconstructed.
	(setq root1 (eroot root2))
	(setq start1 (eqclass root1))

	; break the class back down into two classes
	(xeqclass root1 (eqclass root2)) ; (eqclass root2) is start2
	(xeqclass root2 start1)

	; reflect the diminished size of the first class
	(xeqlength root1 (- (eqlength root1) (eqlength root2)))

	; restore the root pointers of the second class to root2
	(mapeqclass (xeroot x root2) nil (eqclass root2))
	(return nil))) 

(defun emerge (node1 node2) 
  ; (emerge x y) merges the equivalence classes containing x and y,
  ; where x and y are each either atoms or enodes.

  ; First, translate atoms to enode, if necessary.
  (and (atom node1) (setq node1 (enode node1)))
  (and (atom node2) (setq node2 (enode node2)))
  
  (setq emergenumber (1+ emergenumber))

  ; Call emerge2 to perform the merge, making sure that the smaller
  ; class is merged into the larger class.  Then, whenever an enode
  ; is involved in a merge, the size of its class at least doubles,
  ; so that (ignoring context restoration) a node can be involved in at
  ; most log N merges, where N is the maximum number of nodes in the graph.
  (cond ((> (eqlength (eroot node1)) (eqlength (eroot node2)))
	 (emerge1 (eroot node1) (eroot node2)))
	(t (emerge1 (eroot node2) (eroot node1))))

  (setq emergenumber (1- emergenumber))) 

(defun emerge1 (root1 root2) 
  ; merge the equivalence classes c1 and c2 whose roots are root1 and root2,
  ; The size of c1 is assumed to be greater or equal to the size of c2.
  ; root1 will become the root of the new class.
  (prog (start1 start2 tchanged) 
	; if the roots are the same, no action is necessary
	(and (eq root1 root2) (return nil))

	; make root1 the root of every enode in c2
	(mapeqclass (xeroot x root1) nil root2)

	; The elements of every class are circularly linked through
	; their eqclass fields.  Make one big loop out of two little loops,
	; as shown below:
	;
        ;       ******************************************
	;       *        ************************        *
	;       *        V                      *        V
	;  +--> [] --->  [] --+            ---> [] --->  [] --+
	;  |   root1   start1 |    	   |  root2    start2 |
	;  |                  |    	   |                  |
	;  |                  |    	   |                  |
	;  +-- ... <--- [] <--+       	   +-- ... <--- [] <--+
	;
	; (The arrows built from asterisks represent the links that are
        ;  changed to build the big loop.)
	;
	(setq start1 (eqclass root1))
	(setq start2 (eqclass root2))
	(xeqclass root1 start2)
	(xeqclass root2 start1)

	; The size of the new class is equal to the sum of the sizes
	; of the old classes.
	(xeqlength root1 (+ (eqlength root1) (eqlength root2)))

	; Record the merge on the context stack.  For the purposes of
	; popcontext, what was has been done up to now is "the merge".
	; The calls to emerge3 and emerge4 may cause other merges to
	; happen, which will also be recorded.
	(pushcontext (cons popmerge root2))

	; Merge the height values
	(seteheight root1 (min (eheight root1) (eheight root2)))

	; Do something (?) with z
	(emergez root1 (zfield root1) (zfield root2))

	; Merge typewait lists.
	(emergetwait root1 (etypewait root1) (etypewait root2))

	; Handle theory of uninterpreted functions
	(mapclass (emerge3 x start2 root2) nil start1 root1)

	; Merge types.  This may propagate false, enode, or emerge.
    	(setq tchanged (emerget root1 (edatatype root1) (edatatype root2)))

	; Merge demons
	(mapclass (emerge4 x start2 root2) nil start1 root1)
	(mapclass (emerge4 x start1 root1) nil start2 root2)
	 
	; If a type change occured, start any waiting transactions.
	(and tchanged (signaltypechange root1))
	(return t))) 

(defun emerge3 (node1 start2 finish2) 
  ; for every x from start2 to finish2, check for new congruences
  ; between the predecessors of x and the predecessors of node1 and
  ; merge any etype list entries common to x and node1.

  (mapclass (progn (mapctwo (checkcongruent x1 x2)
			    (epredecessors node1)
			    (epredecessors x))
		   (checketype node1 x)
		   (mapctwo (startpattern (car x1) (cdr x1) x2) 
			    (epattern node1) (epredecessors x))
		   (mapctwo (startpattern (car x1) (cdr x1) x2) 
			    (epattern x) (epredecessors node1)))
	    nil
	    start2
	    finish2))

(defunobj popemergedemon (node) (xemergedemon node (cdr (emergedemon node)))) 
       
(defun makemergedemon (node1 node2 fn) 
       (and (eq (eroot node1) (eroot node2)) (firedemon fn node1 nil nil nil))
       (setq makemergenumber (1+ makemergenumber))
       (xemergedemon node1 (cons (cons node2 fn) (emergedemon node1)))
       (pushcontext (cons popemergedemon node1))) 
;;;============================================================================
;;; pattern and rule preprocessing

(defun pfire (fn node matchlist)
       (firedemon (car fn) node matchlist (cadr fn) (caddr fn)))

(defunobj merge* (node pmatchlist lab pattern) 
       (prog (ruletag n1 n2)
             (setq ruletag t)
             (setq n1 (enode (instantiate (car pattern))))
             (and (> (geteheight n1) eheightmax) 
			(rareevent "Merge* stopped by rule depth limit")
			(return nil))
             (setq n2 (enode (instantiate (cdr pattern))))
             (cond ((> (geteheight n2) eheightmax)
			(rareevent "Merge* stopped by rule depth limit"))
                   (t (emerge n1 n2)))))
       
(defunobj split (node matchlist lab pattern) 
       (and slowmode
            (not (> (geteheight node) pheightmax))))

(defunobj poppattern (node) 
	;  Patterns are forcibly cleared here when they are popped.  All
	;  users of patterns must check to see if a pattern element has
	;  been cleared before using it because of this.  This is a fix
	;  for a race condition by Oppen.
       (mapone (rplaca x nil) (car (epattern node)))
       (xepattern node (cdr (epattern node)))) 

(defun nilinpattern (pat)
	;  Check at pattern usage for nil in pattern
    (cond ((memq nil pat) (rareevent "Pattern aborted via nil") t)
	  (t nil)))

(defunobj ifexists (node matchlist lab pattern)
       (prog (f)
	     (and (nilinpattern pattern) (return nil)) ; check for dead pattern
             (setq f (car pattern))
             (cond ((isenode f) (return (pfire (cadr pattern) f matchlist)))
                   ((and (atom f) (not (numberp f)))
                    ((lambda (pmatchlist) (setq f (instantiate f))) matchlist)
                    (return (pfire (cadr pattern) f matchlist)))
                   ((not (isenode (cadr pattern))) (internalerror "ifexists1"))
                   ((not (numberp f)) (internalerror "ifexists2")))
             (setq ifexistsnumber (1+ ifexistsnumber))
             (setq node (cadr pattern))
             (xepattern node (cons (cons pattern matchlist) (epattern node)))
             (pushcontext (cons poppattern node))
             (mapeqclass (mapcone (startpattern pattern matchlist x) 
                                  (epredecessors x)) nil node)
             (return nil))) 

(defunobj ifthen (node matchlist lab pattern)
       (setq ifthennumber (1+ ifthennumber))
       (cond ((> (geteheight node) eheightmax) 
		(rareevent "Ifthen stopped by depth limit")
		(setq heightover (1+ heightover)))
             (t (advance pattern node matchlist (list node) nil))))

(defun makenodedemon (node pattern fn variables)
       (prog (freevariables f)
             (setq freevariables variables)
             (setq f (prepattern pattern))
             (rplacd (last f) (ncons (list fn nil nil)))
             (advance f node nil (list node) nil)))

(defun makedemon (pattern fn variables)
       (prog (freevariables f)
             (setq freevariables variables)
             (setq f (prepattern pattern))
             (rplacd (last f) (ncons (list fn nil nil)))
             (ifexists nil nil nil f)))

(defun prepattern (f)
  ; This function builds an internal representation of the pattern f
  ; (which is intepreted with respect to to global variable freevariables).
  ; The following representation is used.  Atoms in the pattern are replaced
  ; by their enode representations; enodes represent themselves.
  ; Free variables are represented by symbols, and lists are represented
  ; in prefix form, using the rule:
  ;
  ;      REPR[ (x1 x2 ... xn) ] = n REPR[x1] REPR[x2] ... REPR[xn]
  ;
  ; where n is a fixnum.  Note that the representation of any pattern is
  ; a flat list of fixnums, atoms, and enodes.

  (cond ((isenode f) (list f))
	((numberp f) (list (enode f)))
	((null f) (list (enode f)))
	((memq f freevariables) (list f))
	((atom f) (list (enode f)))
	((memq (car f) freevariables) 
	 (internalerror "Function symbol cannot be a free variable"))

	; I believe patterns with singleton lists will bomb the prover
	((= (length f) 1) (internalerror "singleton list in pattern"))
	((atom (car f))
	 (cons (length f) (cons (enode (car f)) 
				(mapcan 'prepattern (cdr f)))))
	(t (cons (length f) (mapcan 'prepattern f)))))

(defun instantiate (f)
       (cond (turnon
              (prog2 (turnoffrules)
                     (instantiate2 f)
                     (turnonrules)))
             (t (instantiate2 f))))

(defun instantiate2 (f)
  ; Given the S-expression f, this function returns an expression with the
  ; same structure, in which atoms that do not represent built-ins (true,
  ; false, omega, predicates, and functions) are replaced by enodes.  Match
  ; variables are replaced by the matching enodes, and other atoms are replaced
  ; by enodes that represent those atoms.
       (cond ((atom f)
              (cond ((cdr (assq f pmatchlist)))
	            ((memq f '(true false omega)) f)
	            (t (setq f (enode f))
                       (cond ((> (geteheight f) eheightmax)
                              (setq instantiatetag t)))
                       f)))
	     ((or (memq (car f) functionnames) (memq (car f) predicatenames))
              (cons (car f) (mapcar 'instantiate2 (cdr f))))
	     ((enode (mapcar 'instantiate2 f)))))

;;;============================================================================
;;; pattern advancing

(defsmac advancepattern ()
  (cond ((null pcar) 
		(internalerror "Null pattern in advancepattern"))
	((equal 0 pcar)
				; I claim this code cannot be executed --Scott
	 (internalerror "advancepattern0") 
	 (cond ((setq q (assq (cadr pcdr) matchlist))
		(cond ((eq (eroot start) (eroot (cdr q)))
		       (advance0 (cddr pcdr) matchlist thread))
		      (t nil)))
	       (t (mapclass (cond ((atom (esuccessors x)))
				  ((econgruent x))
				  ((not (eq (eroot (car pcdr))
					    (eroot (car (esuccessors x))))))
				  (t (advance0 (cddr pcdr)
					       (cons (cons (cadr pcdr) x)
						     matchlist)
					       thread)))
			    nil
			    start
			    finish))))
	((numberp pcar)
	 (mapclass (cond ((atom (esuccessors x)))
			 ((econgruent x))
			 ((not (= (eslength x) pcar)))
			 ((not (eq (eroot (car pcdr))
				   (eroot (car (esuccessors x))))))
			 (t (advance (cdr pcdr)
				     (cadr (esuccessors x))
				     matchlist
				     (cond ((> (eslength x) 2)
					    (cons (cons x 
							(cddr (esuccessors x)))
						  thread))
					   (t thread))
				     x)))
		   nil
		   start
		   finish))
	((atom pcar)
	 (setq q (assq pcar matchlist))
	 (cond ((null q) (advance0 pcdr 
				   (cons (cons pcar finish) matchlist)
				   thread))
	       ((eq (eroot (cdr q)) (eroot start)) (advance0 pcdr 
							     matchlist
							     thread))
	       (t nil)))
	((eq (eroot pcar) (eroot start)) (advance0 pcdr matchlist thread))
	(t nil))) 

(defun startpattern (pattern matchlist node) 
       (cond ((econgruent node))
	     ((null pattern)			; if cleared by poppattern
		(rareevent "Null patttern in startpattern"))
             ((> (geteheight node) eheightmax) 
		(rareevent "Pattern stopped by rule depth limit"))
             ((not (= (car pattern) (eslength node))))
             (t (advance (cddr pattern)
                         (cadr (esuccessors node))
                         matchlist
                         (cond ((> (eslength node) 2)
				(list (cons node (cddr (esuccessors node)))
				      node))
                               (t (list node)))
                         node)))) 

;;;============================================================================
;;; pattern advancing

(defun advance0 (pattern matchlist thread) 
       (cond ((cdr thread)
              (advance pattern
                       (cadar thread)
                       matchlist
                       (cond ((cddar thread)
                              (cons (cons (caar thread)
					  (cddar thread))
				    (cdr thread)))
                             (t (cdr thread)))
                       (caar thread)))
             (t (pfire (car pattern) (car thread) matchlist)))
       t) 

(defunobj popdemon (node) 
       (prog (demon) 
             (setq demon (car (edemon node)))
             (xdemonpattern demon nil)
             (xdemonmatchlist demon nil)
             (xdemonfather demon nil)
             (xdemonthread demon demonavailist)
             (setq demonavailist demon)
             (xedemon node (cdr (edemon node))))) 

(defun advance (pattern finish matchlist thread father) 
       (prog (pcar pcdr start demon q) 
             (setq pcar (car pattern))
             (setq start (eqclass finish))
             (setq pcdr (cdr pattern))
             (and (advancepattern) (return nil))
             (setq demonnumber (1+ demonnumber))
             (setq demon (newdemon))
             (xdemonpattern demon pattern)
             (xdemonmatchlist demon matchlist)
             (xdemonfather demon father)
             (xdemonthread demon thread)
             (xedemon finish (cons demon (edemon finish)))
             (pushcontext (cons popdemon finish))
             (return nil))) 

(defun newdemon nil 
       (prog (demon) 
             (cond (demonavailist (setq demon demonavailist)
                                  (setq demonavailist
					(demonthread demonavailist)))
                   (t (setq demon (alloc-demon))))
             (return demon))) 


;;;============================================================================
;;; demon merging, duplicate demon checking

(defun emerge4 (node start finish) 
       (cond ((> (geteheight node) eheightmax)
		(rareevent "Demon merge stopped by rule depth limit"))
             (t (mapcone (and (car x) (emerge6 node x)) (emergedemon node))
                (mapcone (emerge5 x start finish) (edemon node)))))

(defun emerge5 (demon start finish) 
       (prog (pcar pcdr matchlist thread q) 
             (setq pcar (car (demonpattern demon)))
             (cond ((null (demonfather demon)))
                   ((econgruent (demonfather demon)) (return nil))
                   ((null pcar) (internalerror "emerge5")))
             (setq pcdr (cdr (demonpattern demon)))
             (setq matchlist (demonmatchlist demon))
             (setq thread (demonthread demon))
             (return (advancepattern)))) 

(defunobj remergedemon (pair) (rplaca (car pair) (cdr pair))) 

(defun emerge6 (node demon) 
  ; demon is assumed to be an emergedemon that belongs to node.
  ; Fire the demon if it is eligble to be fired.
  ; Note: an emergedemon is a dotted pair of the form (node1 . f),
  ; and it should be fired (by applying f to node) when node1 is
  ; equivalent to node.
  
  (cond ((not (eq (eroot node) (eroot (car demon)))))
	(t ; Disable the demon so that it will not be fired again,
	   ; and push the change onto the context stack.
	   (pushcontext (cons remergedemon (cons demon (car demon))))
	   (rplaca demon nil) ; will this cause (eroot nil) to be evaluated?

	   ; fire the demon
	   (firedemon (cdr demon) node nil nil nil)))) 

(defun firedemon (fn node matchlist lab cont)
  (funcall fn node matchlist lab cont)) 

;;; The following functions implement a form of rulehandler. Addrule takes a
;;; list of patterns, the free variables of which are defined by the argument
;;; variables. For each instantiation of the free variables which results in 
;;; all the patterns appearing in the E-graph, the formula FN. appropriately
;;; instantiated, is added to the formula. Pattern-matching is done 
;;; semantically; the pattern (f x x) will match (f a b) as long as a = b.
;;; The rulehandler can be thought of as working in parallel with the rest
;;; of the prover; the user should not have to worry about ordering the 
;;; patterns. The order in which rules are entered is irrelevant since the
;;; rulehandler will find all occurrences of all patterns effectively 
;;; simultaneously.

(defun addrule (patterns fn variables rulename)
       (prog (freevariables f)
             (setq freevariables variables)
             (setq f (prepattern (car patterns)))
             (rplacd (last f)
                     (ncons (addrulelist (cadr patterns) (cddr patterns) fn)))
             (ifexists nil nil rulename f)))

(defun addrulelist (pattern l fn)
       (cond (pattern (setq pattern (prepattern pattern))
                      (rplacd (last pattern) 
                              (ncons (addrulelist (car l) (cdr l) fn)))
                      (list 'ifexists rulename pattern))
             (t (list 'applyrule rulename fn))))

(defunobj applypattern (node pmatchlist lab pattern)
          ((lambda (ruletag instantiatetag)
                   (setq pattern (instantiate pattern))
                   (ifexists nil nil lab pattern))
          t nil))

(defunobj applyrule (node pmatchlist lab pattern)
          ((lambda (ruletag instantiatetag)
		(prog (ruledepth)	; local redefinition of ruledepth
		  (setq ruledepth 0)	; calc max rule depth of actuals
	          (and (nilinpattern pattern) (return nil)) ; check for dead pat
		  (mapcone (setq ruledepth 
			(max ruledepth (geteheight (cdr x)))) pmatchlist)
		  (setq ruledepth (+ 1 ruledepth)) ; only place incremented
		  (cond ((> ruledepth eheightmax)   ; if limit reached
			(rareevent "Rule stopped by depth limit 1")
			(return)))		    ; abort rule application
                  (setq pattern (instantiate pattern))
                  (cond (instantiatetag 	; check for eheigtmax exceeded
			    (rareevent "Rule stopped by depth limit 2"))
                        (t (propagaterule lab pattern)))))
          t nil))
;
;	propagaterule  --  rule fully instantiated, propagate it
;
(defun propagaterule (lab pattern)
    (rareevent "Applied rule")			; add to count
    (tallyrule lab)				; tally for diagnosis
    (propagate pattern)				; place rule on propagations
    )

(defunobj ifexists1 (node matchlist lab pattern)
       (prog (f)
	     (and (nilinpattern pattern) (return nil)) ; check for dead pattern
             (setq f (car pattern))
             (cond ((isenode f) (return (pfire (cadr pattern) f matchlist)))
                   ((and (atom f) (not (numberp f)))
                    ((lambda (pmatchlist) (setq f (instantiate f))) matchlist)
                    (return (pfire (cadr pattern) f matchlist)))
                   ((not (isenode (cadr pattern))) (internalerror "ifexists1"))
                   ((not (numberp f)) (internalerror "ifexists2")))
             (setq ifexistsnumber (1+ ifexistsnumber))
             (setq node (cadr pattern))
             (xepattern node (cons (cons pattern matchlist) (epattern node)))
             (pushcontext (cons poppattern node))
             (setq rulenodelist 
                   (cons (cons (car (epattern node)) (caar (epattern node)))
                         rulenodelist))
             (mapeqclass (mapcone (startpattern pattern matchlist x) 
                                  (epredecessors x)) nil node)
             (return nil))) 

(defun addrightrule (patterns fn variables rulename)
       (prog (freevariables f)
             (setq freevariables variables)
             (setq f (prepattern (car patterns)))
             (rplacd (last f)
                     (ncons (addrulelist (cadr patterns) (cddr patterns) fn)))
             (ifexists1 nil nil rulename f)
             (turnoffrules)))


;;;============================================================================
;;; These functions provide a facility for individual provers to store
;;; data in the egraph and to imbed functions to be called when nodes
;;; are merged.  This facility is not used in the system as delivered
;;; by Oppen.

(defunobj popetype (node)
       (xetype node (cdr (etype node))))

(defun addetype (node type datum)
       (xetype node (cons (cons type datum) (etype node)))
       (pushcontext (cons popetype node)))


(defun checketype (node1 node2)
       (and (etype node1)
            (etype node2)
            (mapcone (checketype0 x (etype node2)) (etype node1))))

(defun checketype0 (type l)
       (setq l (assq (car type) l))
       (and l (funcall (car type) (cdr type) (cdr l))))
;;;============================================================================
;;; debugging routines

;;; All of the access functions for extracting fields of enode are implemented
;;; by macros that call cxr.  However, in the case of enumber, we have
;;; to define an actual function as well so that it can be used by mapcar.

(defun nodenumber (x) (enumber x)) 

(defun eprint nil 
       (prog (prinlength prinlevel *nopoint l)
             (setq prinlength nil prinlevel nil *nopoint nil)
             (setq l enodelist)
a            (and (eq l eavailist) (return t))
             (eprint2 l)
             (setq l (eavail l))
             (go a)))

(defun eprint2 (node) 
       (princ '|enumber:|) (princ-tab (enumber node))
       (princ '|econgruent :|) (princ (econgruent node))
       (princ-tab '| |) (princ '|height :|) (princ-tab (eheight node))
       (princ '|esuccess:|)
       (princ-terpri (cond ((atom (esuccessors node)) (esuccessors node))
                           (t (mapcar 'nodenumber (esuccessors node)))))
       (princ '|eroot :|) (princ-tab (enumber (eroot node)))
       (princ '|eqclass :|)
       (princ-tab (enumber (eqclass node)))
       (princ '|eqlength :|)
       (princ-tab (eqlength node))
       (princ '|epreds :|)
       (princ-terpri (mapcar 'nodenumber (epredecessors node)))
       (cond ((zfield node) 
              (princ-tab '|zterm :|)
              (mapcone (princ-tab (zprint x)) (zterm node)) (terpri)))
       (and (epattern node) (print '|Patterns...|))
       (mapcone (patpr x) (epattern node))
       (mapcone (emergeprint x) (emergedemon node))
       (mapcone (demonprint x) (edemon node)) 
       (terpri))

(defun patpr (x) 
  (princ '|pattern :|)
  (princ-terpri (plistify (car x))))

(defun emergeprint (x) 
       (princ '|emergedemon :|)
       (princ-tab (cond ((car x) (enumber (car x))) (t nil)))
       (princ-terpri (cond ((atom (cdr x)) (cdr x)) (t '*)))) 

(defun demonprint (demon) 
       (cond ((demonfather demon)
              (princ '|demonfather:|)
              (princ-tab (enumber (demonfather demon)))))
       (princ-terpri (plistify (demonpattern demon)))
       (princ '|matchlist :|)
       (princ-terpri (mapcar 'demonprint2 (demonmatchlist demon)))
       (princ '|thread :|)
       (princ-terpri (nreverse (demonprint3 (demonthread demon))))) 

(defun demonprint2 (match) (list (car match) (enumber (cdr match)))) 

(defun demonprint3 (thread) 
       (cond ((cdr thread)
              (list (enumber (caar thread)) (mapcar 'nodenumber (cdar thread))))
             (t (list (enumber (car thread)))))) 

(defun zprint (x) (list (caar x) (enumber (cadr x))))
;;;============================================================================
;;; initialization.

(defun esave nil (pushcontext nil) t) 

(defun erestore nil (popcontext nil) t) 

; The simplifier and all its components, including the Egraph, follow a
; stack discipline.  That is, whenver they make a change ot their data
; structures, they save enough state information to be able to restore
; the existing context.  The mechanism used for controlling the saving
; and restoring of contexts is a general purpose one that other provers
; may use.
; 
; Pushcontext takes a single argument, which is either an atom or a
; dotted pair consisting of a function name and an arbitrary data field.
; The argument is pushed onto contextstack.
; 
; Popcontext takes a single argument, which must be an atom.  It pops
; elements off the stack until it reaches an element equal to its
; argument.  For each non-atomic element it pops, it applies the function
; appearing in the first field to the argument list appearing in the
; second field.  Popcontext calls functions in the reverse order that
; pushcontext stacked them.

(defun pushcontext (mark) 
       (setq contextstack (cons mark contextstack))
       (and (not (atom mark)) (null (car mark)) (internalerror "pushcontext"))
       (cond ((atom mark) (epushenv)))) 

(defun popcontext (mark) 
       (prog (x) 
        a    (setq x (car contextstack))
             (setq contextstack (cdr contextstack))
             (cond ((atom x) (epopenv)) (t (funcall (car x) (cdr x))))
             (cond ((eq x mark) (return nil)))
             (go a))) 

(defun einit nil 
       (setq etime 0)
       (setq ruletag nil)
       (setq ruledepth 0)			; current depth into rules
       (setq heightover 0)
       (setq rulenodelist nil)
       (setq enumlist nil)
       (setq demonavailist nil)
       (setq quotednodelist nil)
       (setq contextstack nil)
       (setq demonnumber 0)
       (setq efirednumber 0)
       (setq efiringlist nil)
       (setq ifexistsnumber 0)
       (setq makemergenumber 0)
       (setq ifthennumber 0)
       (setq enumber 1)
       (setq emergenumber 0)
       (setq eduplicatenumber 0)
       (setq enodelist (newenode1 (alloc-enode))) ; a dummy element
       (setq eavailist enodelist)
       (setq truenode (enode 'true))
       (setq falsenode (enode 'false)))

(defun ereset nil (mapcone (remprop x 'enode) (oblist)) (einit)) 

