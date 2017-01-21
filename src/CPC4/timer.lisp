;;;(declare
;;;  (load 'need.o) (load 'princ.o) (load 'map.o))
  
;;;(needs-macros)

(declarespecial time-list ; a list of functions being charged
		  time-stack; a list of function names that is used to 
			    ; keep track of who should currently be charged
		  time-last ; when the function currently being charged,
			    ; which is (car time-stack) began being charged.
         )

; This module is used to figure out where lisp programs are spending
; their time by using the Lister trace package.  The following calls
; are used:
; 
;   (time-divide '(f1 f2 ... ))
;
; This function is used to declare certain functions "special" to the
; timer.  Whenever a special function is entered, runtime is charged
; to that function until that function is left, except for time during
; which that function calls another special function.  Time not charged
; to any special function is charged to "other".  time-divide should
; be called only once, before any other part of this package is called.
;
;   (time-start)
; 
; This function zeroes all the accounts.
;
;   (time-print p)
;
; This function prints elapsed times on port p.  If time-print is called
; outside a special function, the "other" account is updated.

(defun time-update ()
  ; this function charges to the account on the top of the stack all time
  ; spent since the last call to time-update

  (prog (acct curtime)
    (setq acct (car time-stack))  ; the account to be charged
    (setq curtime (ptime))        ; the last tick to be charged to that account

    (putprop acct
	     (mapcar '+ (get acct 'time) (mapcar '- curtime time-last))
	     'time)

    (setq time-last curtime)
    (return nil)))

(defun time-in (func arg)
  ; this function is called when a function is entered.
  ; begin charging that function

  (time-update)
  (putprop func (1+ (get func 'count)) 'count)
  (setq time-stack (cons func time-stack)))

(defun time-out (func arg)
  ; this function is called when a function is left.
  ; stop charging that function and begin charging the previous one

  (time-update)
  (setq time-stack (cdr time-stack)))

(defun time-divide (l)
  ; Have the tracing package calls time-in and time-out for functions whose
  ; names are in l.
  (eval 
   (cons 'trace
	 (mapcar '(lambda (x)
			  (cons x 
				'(traceenter time-in traceexit time-out))) l)))

  ; The above must be done before the following destructive update

  (setq time-list (nconc l '(other)))
  (setq time-stack (ncons 'other))
  (setq time-last (ptime))
  (time-start))

(defun time-start ()
  ; clear all the accounts
  (mapcone (progn (putprop x '(0 0) 'time) (putprop x 0 'count))
	   time-list))

(defun print-sec (s p)
  ; translate s, which is expressed in units of 1/60 seconds to units
  ; of seconds (with two decimal digits) and print the translation on port p
  (print (/ s 60) p)
  (patom '|.| p)
  (setq s (/ (* (mod s 60) 100) 60))
  (print (/ s 10) p)
  (print (mod s 10) p))

(defun time-print (p)
  ; print out all the time accumulated in accounts on port p
  (time-update)
  (patom '|Time spent in| p)
  (terpr p)
  (mapcone (prog (time)
		 (patom '|     | p)
		 (patom x p)
		 (patom '|: | p)
		 (setq time (get x 'time)) 
		 (print-sec (car time) p)
		 (patom '| \ | p)
		 (print-sec (cadr time) p)

		 (patom '| sec.  | p)

		 ; also output call count
		 (print (get x 'count) p)
		 (patom'| calls| p)
		 (terpr p))

	   time-list))
