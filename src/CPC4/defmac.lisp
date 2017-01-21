"@(#)defmac.l	2.4"

;;; defmac (define macro) and defsmac (define simple macro) are like defun,
;;; but they define the function as a macro instead of as an expr.  They are
;;; less powerful then defining a macro directly (for example, they cannot be
;;; used to define macros with arbitrarily many arguments) but are easier to
;;; use.   For example,
;;; 
;;; (defsmac f (x y) (foo (bar x) (bar y)))
;;; 
;;; causes f to be defined as a macro in such a way that every call (f e1 e2)
;;; will expand to (foo (bar e1) (bar e2)) before it is evaluated.
;;; 
;;; defsmac should not be used if the arguments to the macro appear more than
;;; once in the body, or if the order of evaluation of the arguments and the
;;; body is important. For example,
;;; 
;;; (defsmac abs (x) (cond ((> x 0) x) ((-x))))
;;; 
;;; would give an undesirable expansion in a call like (abs (setq x (1+ x))).
;;; (The expansion would increment X twice and return the wrong answer.)
;;; 
;;; In these situations, defmac should be used instead of defsmac.  If a macro
;;; is defined by defmac, calls to it expand to a block which evaluates the
;;; arguments in the macro call, lambda-binds them to generated symbols, and
;;; evaluates and returns the macro body.  For example, after making the
;;; definition:
;;; 
;;; (defmac abs (x) (cons ((> x 0) x) ((-x))))
;;; 
;;; the call (abs (foo 1)) would expand to something equivalent to
;;; 
;;; ((lambda (x) (cond ((> x 0) x) ((- x)))) (foo 1))
;;; 
;;; From this the compiler will produce in-line code to evaluate (foo x) and
;;; take the absolute value of the result.
;;; 
;;; The bodies of the arguments to defmac and defsmac can be implicit progns.
;;;
;;; defsmac and defmac are macros themselves, not fexprs. This means that
;;; if they are called in a file which is being compiled, they will define
;;; the macro at compile time, and no code will be generated. This is almost
;;; always what you want, but you can do a (declare (macros t)) in the
;;; file somewhere if you want the macros defined at run-time, too.

;;;;(declare (macros t)
;;;;	)
;;;;(defun defsmac macro (args)
;;;;       (defsmac1 (cadr args) (caddr args) (cdddr args)))
  
;;;;(defun defmac macro (args)
;;;;       (defmac1 (cadr args) (caddr args) (cdddr args)))

;;;;(defun defsmac1 (name formals body)
;;;;       `(defun ,name macro (app) 
;;;;                     ,(defsmac2 formals
;;;;                                  (cond ((cdr body) (cons 'progn body)) 
;;;;                                        (t (car body))))))

;;;;(defun defsmac2 (formals body)
;;;;       `(sublis  ,(defsmac3 formals 1) (quote ,body)))

;;;;(defun defsmac3 (formals n)
;;;;       (cond ((null formals) nil)
;;;;             (`(cons (cons (quote ,(car formals)) (car ,(defsmac4 n)))
;;;;                           ,(defsmac3 (cdr formals) (1+ n))))))

;;;;(defun defsmac4 (n) (cond ((= n 0) 'app) ((list 'cdr (defsmac4 (1- n))))))

;;;;(defun defmac1 (name formals body)
;;;;       `(defun ,name macro (app)
;;;;                     (list 'do ,(defmac2 formals 1) nil (quote ,(defmac3 body)))))

;;;;(defun defmac2 (formals n)
;;;;       (cond ((null formals) nil)
;;;;             (t `(cons (list (quote ,(car formals)) (car ,(defsmac4 n)))
;;;;                             ,(defmac2 (cdr formals) (1+ n))))))

;;;;(defun defmac3 (body)
;;;;       (cond ((null (cdr body)) (cons 'return body)) 
;;;;            (t (cons (car body) (defmac3 (cdr body))))))

;;; Simplified versions for Common LISP
;;;
;;; These seem to be premature optimzation as macros.

;;;(defmacro defsmac (fname args &rest b)
;;;    `(defun ,fname ,args ,@b))           ; just make it an ordinary function
    
;;;defmacro defmac (fname args &rest b)
;;;  `(defun ,fname ,args ,@b))           ; just make it an ordinary function



;;;;(defun defunobj macro (b)
  ; This macro works just like defun, except that both the value and the
  ; function-binding of the symbol being defined are set to the function
  ; being defined.  Therefore, after (defunobj f ...), (f ...) calls the
  ; defined function, whereas f evaluates to the function itself.
  ;
(defmacro defunobj (fname args &rest b)
  `(progn 
	  (defun ,fname ,args ,@b)
	  (proclaim '(special ,fname)) ;;;; ***declare not allowed here
	  (setq ,fname (function ,fname))))
