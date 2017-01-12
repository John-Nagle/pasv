"@(#)debug.l	2.6"

;;;============================================================================
;;; simpdebug is a macro, that if redefined, will cause debug code
;;; to be included in the simplifier.

;;;;(declare (load 'need.o) (load 'defmac.o) (macros t))
(declarespecial special
	rareevents
	errport)
(needs-macros)

(defmacro simpdebug (x) (comment "debug code is not present"))
;
;	internalerror  --  called if an internal error is detected
;
(defun internalerror (msg)
     (patom "INTERNAL ERROR IN VERIFIER THEOREM PROVER" errport)
     (terpri errport)
     (patom "Message: " errport) 
     (patom msg errport)
     (terpri errport)
     (break "for internal debugging - exit with ctl-D."))
;
;	rareevent  --  tally rare events
;
;	This routine is often traced when debugging performance problems.
;
(defun rareevent (event)
    (prog (tal)
	(setq tal (assoc event rareevents))		; check if on list
	(cond (tal (rplacd tal (+ (cdr tal) 1)))	; tally if in list
	      (t (setq rareevents (cons (cons event 1) rareevents))))
	))
;
;	dumprareevents  --  dump the rare event list
;
(defun dumprareevents nil
    (mapcar 'dumprareevent rareevents)
    nil)
(defun dumprareevent (tal)
    (patom (car tal))				; event name
    (patom ": ")				; event count
    (patom (cdr tal))				; event count
    (terpri))
(setq rareevents nil)					; initialize
