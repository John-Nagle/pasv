"@(#)main.l	2.23"

;;;(declare (load 'defmac.o)
;;;         (load 'map.o))
;;;(needs-macros)

(declarespecial 
	vc-record ; if vc-record is set to a port, unsimplifed
		     ; vcs are pretty-printed to that port.  If the
		     ; variable is nil, no vcs are printed.

	   ER%tpl    ; used to communicate with error handler
	   to-vcg    ; used to send answers back
	   lastvc    ; DEBUG - last VC in case of break
	   errport   ; port on which error messages are output
	   poport    ; standard output - for debug output only
	   shortprint; if true, only failed VCs are printed.
	   rulefilename ; name of rule file to be read at startup
	   $ldprint  ; load message print flag
	   $gcprint  ; garbage collector message print flag
	   dotiming  ; true if timing is to be done
           ruletally ; rule usage counting
           outport   ; used by dumpruletally
	   seriouserrors ; count of major errors (not failed VCs)
		     )

;
;	File names built into simplifier
;
(setq rulefilename 'ruledatabase)		; the rule file
(setq vc-record nil)				; no default logging port

(defun main nil
  ; This function is the interface between the vcg and the simplifier.
  ; The command line is expected to contain two arguments (and possibly
  ; some flags).  The arguments should be one-digit integers, which are
  ; the numeric values of file descriptors that are open for input and
  ; output, respctively.

  ; The function peforms a read/eval loop, reading forms off the input
  ; pipe, evaluating them, and writing the result (followed by a terpr)
  ; back to the output pipe.  The loop is stopped when the form "nil"
  ; is read, or when the input is exhausted.

  (prog
     (args      ; the non-flag arguments to the simplifier
      flags     ; the flags passed to the simplifier
      from-vcg  ; the port from which vcgs are read
      to-vcg    ; the port over which answers are sent
      form      ; the expression read over from-vcg
      )

    ; give lisp lots of memory
      (allocate 'list   750)
      (allocate 'fixnum 100)
      (allocate 'bignum  30)
      (allocate 'hunk2   20)
      (allocate 'hunk3  200)
      (allocate 'hunk4  300)

   ; replace the break-eval-loop error handler with one that just
   ; prints a message and exits.
     (setq ER%tpl '(lambda (l)
			 (mapc '(lambda (x) (patom x errport)) (cdddr l))
			 (terpr)
			 (exit)))

   ; likewise, exit if we get an interrupt from the keyboard
     (signal 2 'exit)

   ; get the arguments and do some checking on them
   (setq args (argnonflags))
   (cond ((memq (length args) '(2 3 4)))
	 (t (patom '|simplifier: arg count|)
	    (terpr)
	    (exit 1)))
   
   ; translate the atoms to ports
   (setq from-vcg (getport (car args) 'r))
   (setq to-vcg   (getport (cadr args) 'w))
   (cond ((null (cddr args)) (setq vc-record nil))
	 (t
	  (setq vc-record (getport (caddr args) 'w))
	  (print-line)))
   
   ; Suppress garbage collector messages
   (setq $gcprint nil)

   ; Load the rule file if any
   (loadrules rulefilename)
   (cond ((portp vc-record) (print-line)))	; separator

   ; the read-eval-print loop
   (do (nil) (nil) ; CL
       (setq form (read from-vcg))
       (or form (return))
       (print (eval form) to-vcg)
       (terpr to-vcg))
   
   ; We are done; we must exit, or the top level, which is main,
   ; will execute again.
   (cond ((> seriouserrors 0)		; if serious errors
	   (terpri errport)
	   (patom "VERIFICATION ABORTED -- " errport)
	   (patom seriouserrors errport)
	   (patom " error" errport)
	   (and (> seriouserrors 1) (patom 's errport)) ; plural
	   (patom "." errport)
	   (terpri errport)
	   (exit 1)))		; fails
   (exit 0)))
     
(declarespecial prove-count leaf-count)
(setq prove-count 0)		; initialize VC count
(setq shortprint t)		; normally just print failed VCs.
(setq dotiming nil)		; timings initially off
(setq seriouserrors 0)		; no serious errors yet
(defun prove (vc)
  ; This function is called by the verification condition generator.
  ; The simplifier is applied to vc, and t or nil is returned to indicate
  ; whether the vc was simplified to true.  If vc-record is nonnil, the vc is
  ; pretty-printed and written out on that port.

  (prog (result elapsed)
	(and (> seriouserrors 0) (return nil)); if any serious errors, fail
	(setq elapsed (+ (car (ptime)) (cadr (ptime)))) ; get start time
	(setq lastvc vc)		;;; DEBUG -- save last VC for debug
	(cond (dotiming (time-start)))	; turn timing on if needed
	(setq prove-count (add1 prove-count)) ; count VCs
					; actually do the proof
	(setq result (eq (simp (normalize vc)) 'true))

					; compute elapsed time
	(setq elapsed (- (+ (car (ptime)) (cadr (ptime))) elapsed)) 
	(cond ((and shortprint result)	; if short listing desired
		(print-result result elapsed)	; print result if desired
		(return result))) 	; do not print successful VCs.
	(cond ((portp vc-record)
	       (pform vc vc-record)
	       (terpr vc-record)
	       (patom '|Verification condition #| errport)
	       (patom prove-count errport)
	       (patom (cond (result '| proved|) (t '| failed|)) errport)
	       (patom "." errport)
	       (terpr errport)
	       (print-result result elapsed)
	       (force-gc) ; we should get back lots of space now.
	       (cond (dotiming (time-print vc-record)))
	       (print-line)))
	
	(return result)))
;
;	print-result  --  print result of verification on vc-record
;
(defun print-result (result elapsed)
    (cond ((portp vc-record)		; if vc-record
	       (dumpruletally vc-record)	; dump rule usage info
	       (patom '|VC #| vc-record)
	       (patom prove-count vc-record)
	       (patom " " vc-record)
	       (patom (cond (result 'proved) (t '|FAILED|)) vc-record)
	       (patom " in " vc-record)
	       (print-sec elapsed vc-record)
	       (patom " seconds. " vc-record)
	       (terpr vc-record)
	       (terpr vc-record))
	  ))

(defun print-line nil
  ; print a line of equals and a blank line to separate vcs
  (patom '|=================================================| vc-record)
  (terpr vc-record)
  (terpr vc-record))

(defun force-gc nil
  ; check storage allocations for all the types used by the prover.
  ; if less than half the allocated space is remaining for a type,
  ; force a garbage collection.
  
  (prog (use)
	(mapcone (progn (setq use (opval x))
			(cond ((> (* 2 (car use)) (* (cadr use) (caddr use)))
			       (gc)
			       (return nil))))
		 '(list hunk3 hunk4 fixnum bignum))))


(defun getport (desc mode)
  ; Translate the symbol to a port.  desc should be one of the symbols
  ; '|0|, '|1|, ... '|9| (it should not be a fixnum).  It should
  ; correspond to a file descriptor that was open when the lisp system
  ; was exec'ed.  mode should be either 'r or 'w, depending on whether
  ; the descriptor was open for reading or writing.

  (setq desc (1- (length (memq desc 
	'(\20 \19 \18 \17 \16 \15 \14 \13 \12 \11 
	  \10 \9 \8 \7 \6 \5 \4 \3 \2 \1 \0)))))
  (and (eq desc -1)
       (patom '|Argument is not a file descriptor|);
       (exit 1))

  (pipeopen desc mode))

(declarespecial startarg)

(defun lconc (tc l)
  ; This function is supposed to be in Franz Lisp as of Sept 81, but does not
  ; seem to be present in the current version.  To parapharse the Sept 81
  ; version of the manual:
  ; 
  ;  (lconc 'tc 'l) 
  ;       WHERE:  tc is a tconc structure and l is a list.
  ;       RETURNS: tc with the list l added to the end.
  ;       NOTE: a tconc structure is a special type of list designed to make
  ;         it easy to add objects to the end.  It consists of a list cell
  ;         whose car points to a list of the elements added with lconc and
  ;         whose cdr points to the last list cell of the list pointed to by
  ;         the car.
  ;   

  (cond ((null (cdr tc)) (rplaca tc l))
	(t (rplacd (cdr tc) l)))

  (cond ((null l))
	(t (rplacd tc (last l))))
  tc)

; tconc also seems to be missing
(defun tconc (tc o) (lconc tc (ncons o)))

(defun argflags nil
  ; This function scans the argument list on the command line of lisp
  ; for flags, returning a list of flags found.  For example, if lisp were
  ; called with the following arguments:
  ;
  ;    lisp -x 10 -y xy -abc
  ;
  ; this function would return the list (x y a b c).
  ; Any argument that does not begin with a minus is ignored.
  ; The first argument scanned (numbered from zero) is startarg.
  ; Normally, startarg should be 1 if this function is being used with
  ; dumplisp, and 3 if this function is being used with autoloading.

  (prog (last    ; number of last argument on command line
	 onelist ; an exploded version of (argv j)
	 arglist ; an accumulated tconc list of flags seen so far
	 )
	
	(setq last (sub1 (argv -1)))
	(setq arglist (ncons nil))
	
	(do (j startarg (add1 j)) ((> j last))
	    (setq onelist (explodec (argv j)))
	    (cond ((eq '|-| (car onelist))
		   (lconc arglist (cdr onelist)))))

	(return (car arglist))))

(defun argnonflags nil
  ; This function scans the argument list on the command line of lisp
  ; returning a list of arguments that are not flags.  For example,
  ; if lisp were called with the following arguments:
  ;
  ;    lisp -x 10 -y xy -abc
  ;
  ; this function would return the list (10 abc).
  ; The first argument scanned (numbered from zero) is startarg.
  ; Normally, startarg should be 1 if this function is being used with
  ; dumplisp, and 3 if this function is being used with autoloading.

  (prog (last    ; number of last argument on command line
	 onearg  ; the "current" argument in the loop
	 arglist ; an accumulated tconc list of args seen so far
	 )
	
	(setq last (sub1 (argv -1)))
	(setq arglist (ncons nil))
	
	(do (j startarg (add1 j))  ((> j last))
	    (setq onearg (argv j))
	    (cond ((eq '|-| (getchar onearg 1)))
		   (t (tconc arglist onearg))))

	(return (car arglist))))
;
;	tallyrule  --  tally rule usage for logging purposes
;
;	An association list of rule names and counts is maintained.
;
(defun tallyrule (rulename)
    (prog (tal)
	(setq tal (assoc rulename ruletally))		; check if on list
	(cond (tal (rplacd tal (+ (cdr tal) 1)))	; tally if in list
	      (t (setq ruletally (cons (cons rulename 1) ruletally))))
	))
;
;	dumpruletally  --  list the rules applied in current proof
;
(defun dumpruletally (outport)
    (mapcar 'dumpruletally1 ruletally)		; dump all rule tally counts
    (setq ruletally nil)			; and reset
    nil)
(defun dumpruletally1 (tal)
    (patom "    Tried " outport)		; "Tried xxx.rule 3 times."
    (patom (car tal) outport)			; rulename name
    (cond ((> (cdr tal) 1)			; times applied if > 1
	   (patom " " outport)
	   (patom (cdr tal) outport)
	   (patom " times" outport)))
    (patom "." outport)
    (terpri outport))
(setq ruletally nil)				; initialize
;
;	debugon  --  turn on debug flags  
;
;	This is for manual use only; it is not invoked in any code.
;
(defun debugon nil
	(cond ((null vc-record) (setq vc-record poport)))
	(setq shortprint nil)		; force long VC listing
	(setq dotiming t)		; timings on 
	(setq $gcprint t)		; garbage collector messages on
	(time-divide '(normalize simp pform))
	)
