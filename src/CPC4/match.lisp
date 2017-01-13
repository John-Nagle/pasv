; match is a macro designed to replace functions like caadar and
; all his brothers.  The call is:
; 
;    (macro 'expr pattern)
; 
; The macro returns (at run time) or nil to indicate whether
; 'expr matches (at run time) the pattern.  Note that the pattern is not
; quoted; it is used at compile time to generate code.  Patterns may contain
; the following:
; 
;   lists:  A list of patterns (P1 P2 ... Pn) matches an expression E if E 
; 	  is a list of exactly n elements and the i'th element of E matches
; 	  Pi.
; 
; 	  A list of the form (P1 P2 ... Pn . T) matches an expression E if
; 	  E has at least n elements, the i'th element of E matches Pi, and
; 	  the sublist obtained by deleting the first n elements of E matches
; 	  T.
; 
;   atoms: These should be variables.  A variable matches anything, and as
;          a side effect, variable is assigned to what it matches.
; 
;   quoted atoms:  These must match literally.
; 
;   asterisks:  These match like variable, but not assignment takes place.
; 
; Here are some examples.
; 
;    (match '((a b c) d e) (A B C))     Sets A to (a b c)
; 					     B to d
; 					     C to e
; 
;    (match '((a b c) d e f) (A B C))   Fails, because the pattern has
;       				more elements than the subject.
; 
;    (match '((a b c) d e f) (A . B))   Sets A to (a b c)
; 					     B to (d e f)
; 
;    (match E (* X . *))                Succeeds if E has at least two
; 					elements; sets X to the second element
; 					when it succees.
; 
;    (match E ('implies! X Y))          Succeeds if E is an implication; sets
; 					X and Y to its operands upon success. 

;;;(declare (macros t))
(defmacro match (expr pattern) 
  `(prog nil ,@(match1 pattern '"" expr '((return t)))))

(defun match1 (pat prefix expr code)
  (cond ((null pat)
	 (cons `(or (null ,(match2 prefix expr)) (return nil)) code))
	((eq pat '*) code)
	((atom pat) (cons `(setq ,pat ,(match2 prefix expr)) code))
	((eq (car pat) 'quote)
	 (cons `(or (equal (quote ,(cadr pat)) ,(match2 prefix expr))
		    (return nil))
	       code))
	(t (cons `(and (atom ,(match2 prefix expr)) (return nil))
		 (match1 (car pat) (concat 'a prefix) expr
			 (match1 (cdr pat) (concat 'd prefix) expr code))))))

(defun match2 (prefix expr)
  (cond ((equal prefix '"") expr)
	(t (list (concat 'c prefix 'r) expr))))
