
;
;	Debugging tools for rule handler
;
;
;	Test case processor
;
;	test tools
;
;	decl  --  declare and print declaration
;
(defun decl (item)
    (remprop (car item) 'vtype)		; cleanup
    (remprop (car item) 'dtype)		; cleanup
    (patom (car item)) (patom ":  ") (patom (cadr item)) (terpri)
    (vardecl (car item) (cadr item)))
;
;	testcase  --  set and save test case
;
(defun testcase (name result form)
    (set name form)			; save under specified name
    (setq testlist (append1 testlist (list name result form))) ; add to test list
	)
;
;	runtests  --  run all test cases
;
(defun runtests nil
    (simpinit)				; reinitialize
    (begin-decl)			; begin junit
    (mapcar 'decl testdecls)		; do all declarations
    (terpri)
    (mapcar 'dolemma testrules)		; do all rules
    (terpri)
    (mapcar 'runtest testlist) 		; try all tests
    (terpri)
    (end-decl) t)			; done
	
;
;	runonetest  --  run one test case, with full reinitialization
;
(defun runonetest (testid)
	(setq testcase (find-if (lambda (x) (eq (car x) testid)) testlist))
	(cond ((null testcase) (break "Test ID not in test list.")))
    (simpinit)				; reinitialize
    (begin-decl)			; begin junit
    (mapcar 'decl testdecls)		; do all declarations
    (terpri)
    (mapcar 'dolemma testrules)		; do all rules
    (terpri)
    (runtest testcase)
    (terpri)
    (end-decl) t)			; done
;
;	runtest  --  run a test case
;
(defun runtest (l)
    (prog (name expected form result)
	(setq name (car l))		; get test name
	(setq expected (cadr l))	; get expected result
	(setq form (caddr l))		; get test case itself.
	(patom "Test case ") (patom name) (patom ": expect ") 
	(patom (cond (expected "true.") (t "false.")))
	(terpri)
	(pform form)			; print form begin simplified
	(setq result (prove form))	; do the test
	(patom result)
	(cond ((not (equal expected result)) ; if fail
	       (patom " -- TEST CASE DID NOT PRODUCE EXPECTED RESULT") (terpri)
	       (break "Test failed."))
	      (t (patom " -- OK") (terpri)))
	(terpri)
	nil))
;
;	Dumping and tracing tools
;
;	dumpanything
;
(defun dumpanything (xx)
    (cond	
		((isenode xx) (patom (dumpenodeexpr xx)))
		((atomp xx) (patom xx))
		((consp xx) (patom "(")
		   ;;;(mapcar 'dumpanything xx) ; ***DOES NOT WORK ON A NON-LIST CONS***
			(dumplist xx)
		   (patom ")"))
		((functionp xx) (patom (getfunctionname xx)))
		(t (patom xx)))
		(patom " ")
		nil)

;
;	dumplist -- also does a single cons
;
(defun dumplist (lst) 
	(cond 	((null lst) (patom "nil"))
				((consp lst) 
					(dumpanything (car lst))
					(cond 	((consp (cdr lst)) (dumplist (cdr lst)))
								((null (cdr lst))) ;	 Nothing to do if null list end
								(t (patom ". ") (dumpanything (cdr lst)))))
				(t (dumpanything(lst)))))
				
;
;	getfunctionname -- there should be some easier way to do this.
;
(defun getfunctionname (fn) 
	(setq s (write-to-string fn))
	(concatenate 'string (subseq s 0 (search "(" s)) ">"))

;
;	dumpbind1  --  dump one pattern-var to expr-var binding
;
(defun dumpbind1 (bind)
	(patom "        ")
	(print (car bind))			; pattern var
	(patom "  <==>  ")			; marker
	(patom (dumpenodeexpr (cdr bind)))	; expression
	(terpri))
;
;	dumpbinds  --  dump all bindings for rule being applied
;
(defun dumpbinds (binds)
	(cond ((null binds) nil)		; if done, exit
	      (t (dumpbind1 (car binds))	; dump binding
		 (dumpbinds (cdr binds)))))	; recurse for rest
;
;	dumpelist  --  dump mixed list of enodes and atoms
;
(defun dumpelist (elist)
	(cond ((atomp elist) (patom elist))
	      ((isenode elist) (patom (dumpenodeexpr elist)))
	      (t	(patom "(")
			(mapc 'dumpelist elist)
			(patom ")")))
	(patom " ")
	nil)
;
;	Trace aids. These are implementation-specific to GNU Common LISP.
;
;	Trace is not standardized in Common LISP.
;
;
;	dumpapplyrule  --  dump bindings
;
(defun dumpapplyrule nil
	(patom "applyrule: ") 
	;;;(patom (arg 3))
	(patom (nth 2 EXT:*TRACE-ARGS*)) ; CL
	(terpri)
	;;;(dumpbinds (arg 2))
	(dumpbinds (nth 1 EXT:*TRACE-ARGS*)); CL
	(terpri)
	)
;
;	Demon dumping
;
(defun dumpfiredemon nil
    (patom "Firing demon ")
    ;;;(patom (arg 1))
	(patom (nth 0 EXT:*TRACE-ARGS*)) ; CL
    (terpri))
	
(defun dumppfire nil
    (patom "Pfire: ")
    (terpri)
    (patom "    Function: ");
    ;(patom (arg 1))
	(dumpanything (nth 0 EXT:*TRACE-ARGS*)) ; CL
    (terpri)
    (patom "    Node: ")
    ;;;(patom (dumpenodeexpr (arg 2)))
	(dumpanything (dumpenodeexpr (nth 1 EXT:*TRACE-ARGS*))) ; CL
    (terpri)
    (patom "    Bindings: ")
    (terpri)
    ;;;(dumpbinds (arg 3))
	(dumpbinds (nth 2 EXT:*TRACE-ARGS*)) ; CL
    (terpri))
;
;	dumppropagate
;
(defun dumppropagate nil
    (patom "Propagate: ")
    ;;;(dumpelist (arg 1))
	(dumpelist (nth 0 EXT:*TRACE-ARGS*)) ; CL
    (terpri))
;
;	dumpemerge
;
(defun dumpemerge nil
    (patom "emerge: ")
    ;;;(cond ((eq (eroot (arg 1)) (eroot (arg 2))) (patom " (ALREADY EQUAL)")))
	(cond ((eq (eroot (nth 0 EXT:*TRACE-ARGS*)) (eroot (nth 1 EXT:*TRACE-ARGS*))) (patom " (ALREADY EQUAL)")))
    (terpri)
    (patom "    ")
    ;;;;(patom (dumpenodeexpr (arg 1)))
	(patom (dumpenodeexpr (nth 0 EXT:*TRACE-ARGS*))) ; CL
    (terpri)
    (patom "    ")
    ;;;;(patom (dumpenodeexpr (arg 2)))
	(patom (dumpenodeexpr (nth 1 EXT:*TRACE-ARGS*))) ; CL
    (terpri)
    nil
	)
;
;	dumppushcontext
;
(defun dumppushcontext nil (patom "pushcontext") (terpri))
(defun dumppopcontext nil (patom "popcontext") (terpri))
;
;	dumpchangedatatype
;
(defun dumpchangedatatype nil
 	(patom "changedatatype: ")
	;;;(patom (dumpenodeexpr (arg 1)))
	(patom (dumpenodeexpr (nth 0 EXT:*TRACE-ARGS*))) ; CL
	(patom " <== ")
	;;;(patom (arg 3))
	(patom (nth 2 EXT:*TRACE-ARGS*))
	(terpri)
	;;;;(and (edatatype (eroot (arg 1))) (not (equal (arg 3)
	;;;;		 (commontype (edatatype (eroot (arg 1))) (arg 3))))
	(and (edatatype (eroot (nth 0 EXT:*TRACE-ARGS*))) (not (equal (nth 2 EXT:*TRACE-ARGS*)
			 (commontype (edatatype (eroot (nth 0 EXT:*TRACE-ARGS*))) (nth 2 EXT:*TRACE-ARGS*))))
	     (break "Less-restrictive changedatatype"))
	nil
	)
;
;	dumpemerget
;
(defun dumpemerget nil
    (prog nil
	;;;;(and (equal (arg 2) (arg 3)) (return))
	(and (equal (nth 1 EXT:*TRACE-ARGS*) (nth 2 EXT:*TRACE-ARGS*)) (return)) ; CL
	(patom "emerget: ")
	;;; (patom (dumpenodeexpr (arg 1)))
	(patom (dumpenodeexpr (nth 0 EXT:*TRACE-ARGS*))) ; CL
	(patom " <== ")
	;;;(patom (arg 2))
	(patom (nth 1 EXT:*TRACE-ARGS*)) ; CL
	(patom "    ")
	;;; (patom (arg 3))
	(patom (nth 2 EXT:*TRACE-ARGS*)) ; CL
	(terpri)
	))
;
;	dumppropagaterule
;
(defun dumppropagaterule nil
    (patom "propagaterule: ")
    ;;;;(patom (arg 1))
	(patom (nth 0 EXT:*TRACE-ARGS*)) ; CL
    (terpri))
;
;	dumpseteheight
;
(defun dumpseteheight nil
  (prog nil
    ;;; (and (equal (eheight (arg 1)) (arg 2)) (return nil)) ; no dump if no change
    (and (equal (eheight (nth 0 EXT:*TRACE-ARGS*)) (nth 1 EXT:*TRACE-ARGS*)) (return nil)) ; no dump if no change
	(patom "seteheight: ")
    ;;; (patom (dumpenodeexpr (arg 1)))
    (patom (dumpenodeexpr (nth 1 EXT:*TRACE-ARGS*))) ; CL
    (patom "     ")
    ;;; (patom (arg 2))
    (patom (nth 1 EXT:*TRACE-ARGS*)) ; CL
	(patom "  (was ")
    ;;; (patom (eheight (arg 1)))
    (patom (eheight (nth 0 EXT:*TRACE-ARGS*))) ; CL
    (patom ")") (terpri)))
;
;	dumpqueuetypewait
;
(defun dumpqueuetypewait nil
	(patom "queuetypewait: ")
	;;; (patom (dumpenodeexpr (arg 1)))
	(patom (dumpenodeexpr (nth 0 EXT:*TRACE-ARGS*))) ; CL
	(patom " ")
	;;; (patom (arg 2))
	(patom (nth 1 EXT:*TRACE-ARGS*)) ; CL
	(patom " ")
	;;; (patom (arg 3))
	(patom (nth 2 EXT:*TRACE-ARGS*)) ; CL
	(terpri)
	)
;
;	dumpsimpinit  --  place to reset trace counters
;
(defun dumpsimpinit nil
   (setq casedepth 0)
	)
;
;	dumpsimpsave  --  entering a new case
;
(defun dumpsimpsave nil
   (patom "simpsave: ")
   (setq casedepth (add1 casedepth))
   (printdepth casedepth) (terpri))
(defun dumpsimprestore nil
   (patom "simprest: ")
   (setq casedepth (- casedepth 1))
   (printdepth casedepth) (terpri))
(defun printdepth (n)
   (cond ((> n 0)
	(patom "| ") (printdepth (- n 1)))))
;
;	dumpsubprovercall
;
(defun dumpsubprovercall nil
   (patom "subprovercall: ")
   ;;; (patom (arg 2))
   (patom (nth 1 EXT:*TRACE-ARGS*))
   (patom "  ")
   ;;; (patom (arg 3))
   (patom (nth 2 EXT:*TRACE-ARGS*))
   (patom "  #")
   ;;; (patom (enumber (arg 1)))
   (patom (enumber (nth 0 EXT:*TRACE-ARGS*)))
   (patom ": ")
   ;;; (patom (dumpenodeexpr (arg 1)))
   (patom (dumpenodeexpr (nth 0 EXT:*TRACE-ARGS*)))
   (terpri))
;
;	dumpnew-eassertz
;
;	Shows all terms going into Z box
;
(defun dumpnew-iassertz nil
	(patom "new-iassertz: ")
	;;; (mapcar 'dumpzterm (arg 1))
	(mapcar 'dumpzterm (nth 0 EXT:*TRACE-ARGS*))
	(terpri))
	
(defun dumpnew-eassertz nil
	(patom "new-eassertz: ")
	;;; (mapcar 'dumpzterm (arg 1))
	(mapcar 'dumpzterm (nth 0 EXT:*TRACE-ARGS*))
	(terpri))
	
(defun dumpzterm (x)
       (patom (caar x))		; constant multiplier
       (patom " * ")		; indicate multiplication
       (patom (dumpenodeexpr (cadr x)))	; term
       (terpri)
       (patom "              ")
	)
;
;	dumppropeq   --   terms coming out of Z box
;	
(defun dumppropeq nil
	(patom "propeq: ")
	;;; (patom (dumpenodeexpr (arg 1)))
	(patom (dumpenodeexpr (nth 0 EXT:*TRACE-ARGS*)))
	(patom "   ")
	;;; (patom (dumpenodeexpr (arg 2)))
	(patom (dumpenodeexpr (nth 1 EXT:*TRACE-ARGS*)))
	(terpri))
;
;	dumpenodeexpr  --  dump enode expression without looking
;			   at root information.
;
(defun dumpenodeexpr (node)
	(cond
		((isenode node) (dumpenodeexpr (esuccessors node)))
		((atomp node) node)
		(t (concat "(" (concatlist (mapcar 'dumpenodeexpr node)) ")"))))
(defun concatlist (lst)
     (cond ((null lst) nil)
	   ((null (cdr lst)) (car lst))
	   (t (concat (car lst) " " (concatlist (cdr lst))))))
;
;	tracetellz
;
(defun dumptellz nil
    (patom "tellz: ")
    ;;; (patom (dumpenodeexpr (arg 2)))
	(patom (dumpenodeexpr (nth 1 EXT:*TRACE-ARGS*))); CL

    (terpri))
;
;	traceprepattern  --  dump pattern before and after processing
;
;
(defun dumpprepattern nil
    (setq prepatterndepth (+ 1 prepatterndepth))
    (cond ((equal prepatterndepth 1)
    	(patom "prepattern: ")
    	;;; (patom (dumpenodeexpr (car fargs)))
    	(patom (dumpenodeexpr (nth 0 EXT:*TRACE-ARGS*)))
    	(terpri))
	(t nil)))
	
(defun dumpexitprepattern nil
    (setq prepatterndepth (- prepatterndepth 1))
    (cond ((equal prepatterndepth 0)
    	(patom "Exiting prepattern: ")
    	;;; (patom (dumpenodeexpr fresult))
    	(patom (dumpenodeexpr EXT:*TRACE-VALUES*))
     	(terpri))
	(t nil)))
;
;	These turn on tracing of the indicated function
;
(defun tracefiredemon nil (trace (firedemon :pre (dumpfiredemon))))
(defun tracepfire nil (trace (pfire :pre (dumppfire))))
(defun traceapplyrule nil (trace (applyrule :pre (dumpapplyrule))))
(defun tracecontext nil (trace (pushcontext :pre (dumppushcontext)))
			(trace (popcontext :post (dumppopcontext))))
(defun tracepropagate nil (trace (propagate :pre (dumppropagate))))
(defun traceemerge nil (trace (emerge :pre (dumpemerge))))
(defun tracechangedatatype nil (trace (changedatatype :pre (dumpchangedatatype))))
(defun traceemerget nil (trace (emerget :pre (dumpemerget))))
(defun tracepropagaterule nil (trace (propagaterule :pre (dumppropagaterule))))
(defun traceseteheight nil (trace (seteheight :pre (dumpseteheight))))
(defun tracequeuetypewait nil (trace (queuetypewait :pre (dumpqueuetypewait))))
(defun tracesubprovercall nil (trace (subprovercall :pre (dumpsubprovercall))))
(defun tracenew-iassertz nil (trace (new-iassertz :pre (dumpnew-iassertz))))
(defun tracenew-eassertz nil (trace (new-eassertz :pre (dumpnew-eassertz))))
(defun tracepropeq nil (trace (propeq :pre (dumppropeq))))
(defun tracetellz nil (trace (tellz :pre (dumptellz))))
(defun traceze nil (tracenew-iassertz) (tracenew-eassertz) 
			(tracetellz) (tracepropeq))
(defun traceprepattern nil (setq prepatterndepth 0)
			(trace (prepattern 
			:pre (dumpprepattern)
			:post (dumpexitprepattern))))
(defun tracecasing nil 
	(trace (simpsave :pre (dumpsimpsave)))
	(trace (simprestore :pre (dumpsimprestore)))
	(trace (simpinit :pre (dumpsimpinit))))
(setq prinlevel 5 prinlength 5)		; avoid runaway printing
