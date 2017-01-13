;"@(#)setup.l	2.15"
;"@(#)setup.l	2.1"

;;;
;;;  Load theorem prover
;;;

; needs-macros is called in every file that uses macros.
; It is also defined as a macro that expands to a comment in every
; file that uses macros.  Thus, if all the macros have been expanded
; at compile time, the function is not called.  However, if uncompiled
; functions are being loaded, they will call this function, which
; will load in all the macro files.

;;;;(declare (special macros-loaded)) ;;; Not Common LISP
(setq macros-loaded nil)
;;;;

(defun needs-macros () 
    (and (not macros-loaded)
      (setq macros-loaded t)
       (mapc 'load '(clispcompat.lisp defmac.lisp hunkshell.lisp debug.lisp enode.lisp map.lisp princ.lisp progvn.lisp match.lisp))))
       
(needs-macros)  ; bring in all needed macros
      


; Clear the translink table so that franz-top-level will use
; the newly defined print
;;;;(sstatus translink nil) ;;; Not Common LISP

; Control how deep rule instantiation will go
(setq  eheightmax 1)			; only 1 rule deep
(setq  pheightmax 4)

; load the theorem prover
(load "pp.lisp")
(*ppinit 72)
(load "pform.lisp")
(load "z.lisp")
(initz)

(load "ze.lisp")
(load "e.lisp")
(load "eform.lisp")
(load "newsimp.lisp")
(load "main.lisp")
(load "newsplit.llisp")
(load "normalize.lisp")
(load "builtin.lisp")
(load "generic.lisp")
(load "timer.lisp")
(load "ruleprep.lisp")
(load "eventtran.lisp")
(load "type.lisp")
(load "typee.lisp")
(load "fixes.lisp")	;;; ***TEMP** these functions won't compile right on SUNs!
(cfasl 'pipeopen.o '_Lpipeopen 'pipeopen '"function" '"")

(setq quotednamelist nil)
(setq constantnames '(nil true false omega))
(setq functionnames '(addi! subi! muli! divi! modi! storea! selecta! storer!
		      selectr!))
(setq predicatenames '(lei! gei! lti! gti!))
(definearithmetic addi! subi! muli! gei! gti! lei! lti!)
(defineboolean)
(definejsyntax)
(simpinit)
(setq prinlevel nil)		;; no summarizing - will break VCG if set
(setq prinlength nil)
(sstatus translink on)
