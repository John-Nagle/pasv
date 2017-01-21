;
;	Built-in knowledge of the prover
;	The demons defined here are accepted as axioms by the prover.
;	The theory of arrays and of records is defined here, as
;	are type predicates.  We also provide some information
;	about division and remainder.
;
;	Demons which match on symbols known in the Boyer-Moore system
;	perform type checking, so that no rule will be applied outside
;	the domain in which Boyer-Moore semantics and the semantics
;	of the rules here are identical.  In general, anything which
;	is used in an arithmetic sense must be shown to be an integer.
;
;						Version 1.24 of 2/6/83
;
;;;(declare
;;;  (load 'need.o) (load 'defmac.o) (load 'hunkshell.o) (load 'enode.o))

;;;(needs-macros)
(declarespecial 
	daddn
	darraycompactstore
	darraypdef
	darraypstore
	darraysplit
	darraytrivialstore
	dbooleanpdef
	ddiv
	ddivn
	dgtn
	dintegerpdef
	dmod
	dmuli
	dnumberpdef
	drecordcompactstore
	drecordsplit
	drecordtrivialstore
	dsubn
	boolsymand
	boolsymor
	boolsymimplies
	boolsymeq
	boolsymnot
	truenode
	falsenode
	integertype
	booleantype
	arraytype
	universaltype
	)
;
;	numberptype  --  true if variable is subrange from 0 to N
;
(defun numberptype (n)
    (prog (ntype)
	(setq ntype (getdatatype n))		; get type of n
	(return (and (eq (car ntype) 'subrange)	; if subrange
		     (cadr ntype)		; and not nil low bound
		     (not (minusp (cadr ntype))))))) ; and not negative 
;
;	Type predicates
;
;	integerp!  --  true if object is integer
;
(defunobj dintegerpdef (node matchlist lab pattern)
    (prog (n ntype)
	(and (dtwait 'dintegerpdef node matchlist lab pattern) (return)) ; typ ck
	(setq n (cdr (assq 'n matchlist)))	; get object of numberp
	(setq ntype (getdatatype n))		; get type of n
	(cond ((or (eq (car ntype) (car integertype)) ; if integer
		   (eq (car ntype) 'subrange)); or subrange
		(emerge node truenode)		; predicate is true
		(return))
	      (t 				; otherwise
		(emerge node falsenode)))))	; predicate is false
;
;	booleanp! --  true if object is boolean
;
(defunobj dbooleanpdef (node matchlist lab pattern)
    (prog (n ntype)
	(and (dtwait 'dbooleanpdef node matchlist lab pattern) (return)) ; typ ck
	(setq n (cdr (assq 'n matchlist)))	; get object of numberp
	(setq ntype (getdatatype n))		; get type of n
	(cond ((eq (car ntype) (car booleantype)) ; if boolean
		(emerge node truenode)		; predicate is true
		(return))
	      (t 				; otherwise
		(emerge node falsenode)))))	; predicate is false
;
;	arrayp!  --  true if object is array
;
(defunobj darraypdef (node matchlist lab pattern)
    (prog (n ntype)
	(and (dtwait 'darraypdef node matchlist lab pattern) (return)) ; typ ck
	(setq n (cdr (assq 'n matchlist)))	; get object of arrayp
	(setq ntype (getdatatype n))		; get type of n
	(cond ((eq (car ntype) 'array) 		; if array
		(emerge node truenode)		; predicate is true
		(return))
	      (t 				; otherwise
		(emerge node falsenode)))))	; predicate is false
;
;	Definitions of some built-in objects.
;
;	Definition of "numberp":  (natural number)
;
;	(implies! (integerp! node)
;		  (equal! (gei! n 0) (numberp! n)))
;
;	Most of the time we can deduce numberp! from
;	subrange information.
;
(defunobj dnumberpdef (node matchlist lab pattern)
  (prog (n ntype)
	(and (dtwait 'dnumberpdef node matchlist lab pattern) (return)) ; typ ck
	(setq n (cdr (assq 'n matchlist)))
	(setq ntype (getdatatype n))		; type of n
	(cond ((and (eq (car ntype) 'subrange)	; if subrange
		    (not (lessp (cadr ntype) 0))) ; and low bound >= 0
		(emerge node truenode)		; must be numberp
		(return))			; true
	      ((or (eq (car ntype) 'subrange)	; if integer
		   (eq (car ntype) (car integertype))) 
			(emerge node		; merge with defn
			    (enode (list 'gei! n 0))))
	      (t (emerge node falsenode)))))	; non-integer - fails
;
;	(arrayp! (storea! a i e)) is true by definition.
;
;	Applied only when a is array and i is integer or subrange.
;
(defunobj darraypstore (node matchlist lab pattern)
    (prog (a i e)
	(and (dtwait 'darraypstore node matchlist lab pattern) (return)) ; typ ck
	(setq a (cdr (assq 'a matchlist)))
	(setq i (cdr (assq 'i matchlist)))
	(setq e (cdr (assq 'e matchlist)))
	(and (not (equal (car (getdatatype a)) 'array)) (return)) ; a array?
	(and (not (integerptype i))  (return)) ; i integer?
	(emerge node truenode)))
;
;	addn!  --  add for nonnegative numbers, defined only when
;		   both operands are nonnegative.
;
;	(implies! (and! (gei! i 0) (gei! j 0))
;		  (equal! node (addi! i j)))
;
;	Applied only when i and j are integers.
;
(defunobj daddn (node matchlist lab pattern)
    (prog (i j)
	(and (dtwait 'daddn node matchlist lab pattern) (return)) ; typ ck
	(setq i (cdr (assq 'i matchlist)))
	(setq j (cdr (assq 'j matchlist)))
	(and (not (integerptype i)) (return)) ; i integer?
	(and (not (integerptype j)) (return)) ; j integer?
	(cond ((and (numberptype i) (numberptype j)) ; if both are known numberp
		(emerge node (enode (list 'addi! i j)))	; immediately use addi!
		(return))
	      (t (propagate (list boolsymimplies	; otherwise hard way
			(list boolsymand
				(list 'gei! i 0)
				(list 'gei! j 0))
			(list boolsymeq node (list 'addi! i j))))))))
;
;	subn!  --  subtract for nonnegative numbers, defined only when
;		   both operands are nonnegative and
;		   the result is nonnegative
;
;	(implies! (and! (gei! i j) (and! (gei! i 0) (gei! j 0)))
;		  (equal! node (subi! i j)))
;
;	Applied only when i and j are integers.
;
(defunobj dsubn (node matchlist lab pattern)
    (prog (i j)
	(and (dtwait 'dsubn node matchlist lab pattern) (return)) ; typ ck
	(setq i (cdr (assq 'i matchlist)))
	(setq j (cdr (assq 'j matchlist)))
	(and (not (integerptype i))  (return)) ; i integer?
	(and (not (integerptype j))  (return)) ; j integer?
	(propagate (list boolsymimplies
			(list boolsymand
			    (list 'gei! i j)
			    (list boolsymand 
				(list 'gei! i 0)
				(list 'gei! j 0)))
			(list boolsymeq node (list 'subi! i j))))))
;
;	divn!  --  div for nonnegative numbers, defined only when
;		   both operands are nonnegative.
;
;	(implies! (and! (gei! i 0) (gei! j 0))
;		  (equal! node (divi! i j)))
;
;	Applied only when i and j are integers.
;
(defunobj ddivn (node matchlist lab pattern)
    (prog (i j)
	(and (dtwait 'ddivn node matchlist lab pattern) (return)) ; typ ck
	(setq i (cdr (assq 'i matchlist)))
	(setq j (cdr (assq 'j matchlist)))
	(and (not (integerptype i)) (return)) ; i integer?
	(and (not (integerptype j)) (return)) ; j integer?
	(cond ((and (numberptype i) (numberptype j)) ; if both are known numberp
		(emerge node (enode (list 'divi! i j)))	; immediately use divi!
		(return))
	      (t (propagate (list boolsymimplies	; otherwise hard way
			(list boolsymand
				(list 'gei! i 0)
				(list 'gei! j 0))
			(list boolsymeq node (list 'divi! i j))))))))
;
;	gtn!  --  greater than for natural numbers
;
;	As above, this is defined for nonnegative numbers only.
;
;	(implies! (and! (gei! i 0) (gei! j 0))
;		  (equal! node (gti! i j)))
;
;	Applied only when i and j are integers.
;
(defunobj dgtn (node matchlist lab pattern)
    (prog (i j)
	(and (dtwait 'dgtn node matchlist lab pattern) (return)) ; typ ck
	(setq i (cdr (assq 'i matchlist)))
	(setq j (cdr (assq 'j matchlist)))
	(and (not (integerptype i))  (return)) ; i integer?
	(and (not (integerptype j))  (return)) ; j integer?
	(cond ((and (numberptype i) (numberptype j)) ; if both are known numberp
		(emerge node (enode (list 'gti! i j)))	; immediately use gti!
		(return))
	      (t (propagate (list boolsymimplies	; otherwise hard way
			(list boolsymand
				(list 'gei! i 0)
				(list 'gei! j 0))
			(list boolsymeq node (list 'gti! i j))))))))
;
;	muli!  --  integer multiply
;
;	This looks only at the subranges of the operands and computes
;	the subranges of the results.  
;
;	Effects:
;
;	1.  Where x and y are equal to constants, but not explicit constants,
;	    the value of x*y will be calculated and merged.
;
;	2.  Bounds for x*y will be generated in all cases where bounds are
;	    available for x and y.
;
;	3.  Should the bounds for x or y change, the demon will be refired,
;	    which will result in a recalculation.
;
(defunobj dmuli (node matchlist lab pattern)
    (prog (i j)
	(and (dtwait 'dmuli node matchlist lab pattern) (return)) ; typ ck
	(setq i (cdr (assq 'i matchlist)))
	(setq j (cdr (assq 'j matchlist)))
	(and (not (integerptype i)) (return)) ; i integer?
	(and (not (integerptype j)) (return)) ; j integer?
	;	Calculate worst-case bounds based on subrange info
	(emerget node (getdatatype node)
		     (typefrombounds (mulibounds
			(getbounds (getdatatype i))
			(getbounds (getdatatype j)))))
	;	If type of either arg changes, redo this calculation.
	(queuetypewait i 'dmuli (list node matchlist lab pattern))
	(queuetypewait j 'dmuli (list node matchlist lab pattern))
	))
;
;	mulibounds  --  calculate bounds for result of multiply
;
;	Bounds are from
;
;	    min(a.lo * b.lo, a.hi * b.hi, a.lo * b.hi, a.hi * b.lo)
;	to
;	    max(a.lo * b.lo, a.hi * b.hi, a.lo * b.hi, a.hi * b.lo)
;
;	Note that this calculation cannot result in void bounds.
;
(defun mulibounds (b1 b2)
     (prog (lo1 lo2 hi1 hi2)
	(setq lo1 (car b1))		; get low bound 1
	(setq lo2 (car b2))		; get low bound 2
	(setq hi1 (cdr b1))		; get high bound 1
	(setq hi2 (cdr b2))		; get high bound 2
	(cond ((not (and lo1 lo2 hi1 hi2)) (return nil))) ; fail if any nil
	;	Calculate bounds of result
	(return (cons (min (multiply lo1 lo2) (multiply hi1 hi2)
			   (multiply lo1 hi2) (multiply hi1 lo2))
		      (max (multiply lo1 lo2) (multiply hi1 hi2)
			   (multiply lo1 hi2) (multiply hi1 lo2))))
	))
;
;	Division and remainder by constants
;
;	The rules applied here are
;
;	e = a mod b 
;    generates
;	a >= 0 and b > 0 implies e <= b - 1
;	a >= 0 and b > 0 implies e >= 0
;	
;	e = a div b
;    generates
;	a >= 0 and b > 0 implies a >= e
;	a >= 0 and b > 0 implies e >= 0
;	a >= 0 and b > 0 implies e*b <= a
;	a >= 0 and b > 0 implies e*b + b >= a + 1
;
;	(We will add rules for negative div and mod later).
;
;
;	division demon
;
(defunobj ddiv (node matchlist lab pattern)
  (prog (a b)
	(and (dtwait 'ddiv node matchlist lab pattern) (return)) ; typ ck
	(setq a (cdr (assq 'a matchlist)))
	(setq b (cdr (assq 'b matchlist)))
	(and (not (integerptype a))  (return)) ; a integer?
	(and (not (integerptype b))  (return)) ; b integer?

	;	a >= 0 and b > 0 implies a >= e
	(propagate (list boolsymimplies
			(list boolsymand
			    (list 'gei! a 0)
			    (list 'gti! b 0))
			(list 'gei! a node)))

	;	a >= 0 and b > 0 implies e >= 0
	(propagate (list boolsymimplies
			(list boolsymand
			    (list 'gei! a 0)
			    (list 'gti! b 0))
			(list 'gei! node 0)))

	;	a >= 0 and b > 0 implies a >= e * b
	(propagate (list boolsymimplies
			(list boolsymand
			    (list 'gei! a 0)
			    (list 'gti! b 0))
			(list 'gei! a
			    (list 'muli! node b))))

	;	a >= 0 and b > 0 implies (b + e*b) >= (a + 1)
	(propagate (list boolsymimplies
			(list boolsymand
			    (list 'gei! a 0)
			    (list 'gti! b 0))
			(list 'gei! 
			    (list 'addi! b (list 'muli! node b))
			    (list 'addi! a 1))))
	))
;
;	remainder demon
;
(defunobj dmod (node matchlist lab pattern)
  (prog (a b)
	(and (dtwait 'dmod node matchlist lab pattern) (return)) ; typ ck
	(setq a (cdr (assq 'a matchlist)))
	(setq b (cdr (assq 'b matchlist)))
	(and (not (integerptype a))  (return)) ; a integer?
	(and (not (integerptype b))  (return)) ; b integer?

	;	a >= 0 and b > 0 implies (b - 1) >= e
	(propagate (list boolsymimplies
			(list boolsymand
			    (list 'gei! a 0)
			    (list 'gti! b 0))
			(list 'gei! (list 'subi! b 1)
			    node)))

	;	a >= 0 and b > 0 implies e >= 0
	(propagate (list boolsymimplies
			(list boolsymand
			    (list 'gei! a 0)
			    (list 'gti! b 0))
			(list 'gei! node 0)))
	))
;
;	Array prover
;
;	We must check that subscripts are integers, but we need not
;	check that arrays are array type, because that checking is
;	performed in typeselecta.
;
;	NOTE POSSIBLE SOUNDNESS PROBLEM:
;	     The initial version of the rule builder has an array theory
;	     which supports only arrays with positive subscripts.  The
;	     definition of storea! in the rule builder is such that
;
;	        (storea! A -1 V) = (storea! A -1 W)
;
;	     even when V <> W, which is false here.  However, since
;	     pass 2 of the verifier will trap arrays declared with negative
;	     bounds, this should not be a problem.
;
(defunobj darraytrivialstore (node matchlist lab pattern)
  (prog (a i)
	(and (dtwait 'darraytrivialstore node matchlist lab pattern) (return)) 
	(setq a (cdr (assq 'a matchlist)))
	(setq i (cdr (assq 'i matchlist)))
	(and (not (integerptype i))  (return)) ; i integer?
  	(emerge node (cadr (esuccessors node)))))

(defunobj darraycompactstore (node matchlist lab pattern)
  (prog (a i f)
	(and (dtwait 'darraycompactstore node matchlist lab pattern) (return)) 
								; typ ck
	(setq a (cdr (assq 'a matchlist)))
	(setq i (cdr (assq 'i matchlist)))
	(setq f (cdr (assq 'f matchlist)))
	(and (not (integerptype i))  (return)) ; i integer?
	(emerge node 
		(enode (list (car (esuccessors node)) a i f))))) 

(defunobj darraysplit (node matchlist lab pattern)
  (prog (a i e j) 
	(and (dtwait 'darraysplit node matchlist lab pattern) (return)) ; typ ck
	(setq a (cdr (assq 'a matchlist)))
	(setq e (cdr (assq 'e matchlist)))
	(setq i (cdr (assq 'i matchlist)))
	(setq j (cdr (assq 'j matchlist)))
	(and (not (integerptype i))  (return)) ; i integer?
	(and (not (integerptype j))  (return)) ; j integer?
	(and (eq (eroot i) (eroot j)) (return (emerge node e)))
	(propagate (list boolsymor 
			 (list boolsymand
                               (list boolsymeq i j)
			       (list boolsymeq e node))
                         (list boolsymand
                               (list boolsymnot (list boolsymeq i j))
			       (list boolsymeq node (list 'selecta! a j))))))) 

;
;	record prover
;
;	No type checking is required in the record prover, because its
;	function symbols are unknown in the Boyer-Moore system and thus
;	no type-induced inconsistency can result.
;
(defunobj drecordtrivialstore (node matchlist lab pattern)
  (prog (a i)
	(setq a (cdr (assq 'a matchlist)))
	(setq i (cdr (assq 'i matchlist)))
  	(emerge node (cadr (esuccessors node)))))

(defunobj drecordcompactstore (node matchlist lab pattern)
  (prog (a i f)
	(setq a (cdr (assq 'a matchlist)))
	(setq i (cdr (assq 'i matchlist)))
	(setq f (cdr (assq 'f matchlist)))
	(emerge node 
		(enode (list (car (esuccessors node)) a i f))))) 
;
;	drecordsplit  --  select of store of record
;
(defunobj drecordsplit (node matchlist lab pattern)
  (prog (a i e j) 
	(setq a (cdr (assq 'a matchlist)))
	(setq i (cdr (assq 'i matchlist)))
	(setq j (cdr (assq 'j matchlist)))
	(setq e (cdr (assq 'e matchlist)))
	(cond ((eq i j) (emerge node e))
	      (t (emerge node (enode (list 'selectr! a j)))))))
;
;	Patterns  --  these fire when the pattern matches and
;	start the indicated routine.
;
(defun initbuiltin nil
  (makedemon '(integerp! n) dintegerpdef '(n))
  (makedemon '(booleanp! n) dbooleanpdef '(n))
  (makedemon '(arrayp! n) darraypdef '(n))
  (makedemon '(numberp! n) dnumberpdef '(n))
  (makedemon '(arrayp! (storea! a i e)) darraypstore '(a i e))
  (makedemon '(addn! i j) daddn '(i j))
  (makedemon '(subn! i j) dsubn '(i j))
  (makedemon '(divn! i j) ddivn '(i j))
  (makedemon '(gtn! i j) dgtn '(i j))
  (makedemon '(muli! i j) dmuli '(i j))
  (makedemon '(mod! a b) dmod '(a b))
  (makedemon '(divi! a b) ddiv '(a b))
  (makedemon '(storea! a i (selecta! a i)) darraytrivialstore '(a i))
  (makedemon '(storea! (storea! a i e) i f) darraycompactstore '(a i e f))
  (makedemon '(selecta! (storea! a i e) j) darraysplit '(a i e j))
  (makedemon '(storer! a i (selectr! a i)) drecordtrivialstore '(a i))
  (makedemon '(storer! (storer! a i e) i f) drecordcompactstore '(a i e f))
  (makedemon '(selectr! (storer! a i e) j) drecordsplit '(a i e j))
   	)
