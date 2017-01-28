"@(#)pform.l	2.2"

;;; A quick and dirty unparser for verifier formulas.

;;;;(declare (load 'need.o) (load 'defmac.o) (load 'map.o)) ; CL

(needs-macros)

(setq *nopoint t)

(defun ppset (name p pval plen associative)
       (putprop name p 'p)
       (putprop name pval 'pval)
       (putprop name plen 'plen)
       (putprop name associative 'asso))


(mapcone (apply 'ppset x)
         '((assign!      5    ":="        2   nil)
          (if!           10   nil         2   nil)
          (implies!      20   "implies"   7   nil)
          (or!           30   "or"        2   t  )
          (and!          40   "and"       3   t  )
          (not!          50   "not"       3   nil)
          (equal!        20   "="         1   t  )
          (notequal!     20   "<>"        2   nil)
          (lei!          20   "<="        2   nil)
          (gti!          20   ">"         1   nil)
          (notimplies!   20   ">"         1   nil)
          (gei!          20   ">="        2   nil)
          (impliedby!    20   ">="        2   nil)
          (lti!          20   "<"         1   nil)
          (notimpliedby! 20   "<"         1   nil)
          (addi!         30   "+"         1   nil)
          (subi!         30   "-"         1   nil)
          (muli!         40   "*"         1   t  )
          (divide!       40   "/"         1   nil)
          (divi!         40   "div"       3   nil)
          (mod!          40   "mod"       3   nil)
	  (negi!	 50   "-"	  1   nil)))
(defsmac leftparen (p)
         (cond ((> p (get (car x) 'p))
                (*ppbegin 1)
                (*ppstring '|(| 1))))

(defsmac rightparen (p)
         (cond ((> p (get (car x) 'p))
                (*ppstring '|)| 1)
                (*ppend))))

(declarespecial poport)
		  
(defun pform (f &optional (port nil))
  ; (pform f [port]) pretty-prints f, writing the output on port,
  ; if given, otherwise on the standard output port.
  
  ; The purpose of the lambda is to temporarily bind poport to the
  ; port passed as an argument.  The pp routines send their output
  ; to poport.
  ((lambda (poport)
	   (*ppbegin 0)
	   (ppform f 0)
	   (*ppend)
	   (*ppeof))

   (cond (port port) 		; if port given, use it
	         (t poport))))	; otherwise use default


(defun ppform (x p)
  (*ppbegin 0)
  (cond ((numberp x) 
	 ((lambda (s)(*ppstring (implode s) (length s))) (explodec x)))
	
	((atomp x) (*ppstring x (flatc x)))
	
	((eq (car x) 'consti!)
	 ((lambda (s)(*ppstring (implode s) (length s)))
	  (explodec (cadr x))))
	
	((memq (car x) '(equal! notequal! lei! lti! gei! gti! addi!
				muli! subi! divide! divi! mod! assign!
				notimplies! impliedby! notimpliedby!))
	 (leftparen p)
	 (cond ((> (length x) 2)
		(ppnary (car x) (cdr x) nil 2 1))
	       ((and (eq (length x) 1)
	            (eq (car x) 'subi!))
		(*ppstring (get 'subi! 'pval) (get 'subi! 'plen))
		(ppform (cadr x) (get (car x) 'p)))
	       (t
		(*ppstring '|?| 1)))
	 (rightparen p))
	
	((eq 'implies! (car x))
	 (leftparen p)
	 (ppform (cadr x) (get 'implies! 'p))
	 (*ppblank t 0 1)
	 (*ppstring (get 'implies! 'pval) (get 'implies! 'plen))
	 (*ppblank t 2 1)
	 (ppform (caddr x) (get 'implies! 'p))
	 (rightparen p))
	
	((eq 'if! (car x))
	 (leftparen p)
	 (*ppstring '|if| 2)
	 (*ppblank nil 2 1)
	 (ppform (cadr x) (get 'if! 'p))
	 (*ppblank t 2 1)
	 (*ppstring '|then| 4)
	 (*ppblank t 2 1)
	 (ppform (caddr x) 0)
	 (*ppblank t 2 1)
	 (*ppstring '|else| 4)
	 (*ppblank t 2 1)
	 (ppform (cadddr x) (get 'if! 'p))
	 (rightparen p))

	((memq (car x) '(and! or!))
	 (leftparen p)
	 (ppnary (car x) (cdr x) t 0 1)
	 (rightparen p))

	((memq (car x) '(negi! not!))
	 (leftparen p)
	 (*ppstring (get (car x) 'pval) (get (car x) 'plen))
	 (and (eq (car x) 'not!) (*ppblank t 2 1))
	 (*ppbegin 1)
	 (ppform (cadr x) (get (car x) 'p))
	 (*ppend)
	 (rightparen p))

	((eq (car x) 'storea!)
	 (*ppbegin 1)
	 (*ppstring '|<| 1)
	 (*ppblank nil 1 0)
	 (ppform (cadr x) 0)
	 (*ppblank nil 1 0)
	 (*ppstring '|,| 1)
	 (*ppblank nil 0 0)
	 (ppform (caddr x) 0)
	 (*ppblank nil 1 0)
	 (*ppstring '|,| 1)
	 (*ppblank nil 0 0)
	 (ppform (cadddr x) 0)
	 (*ppblank nil 2 0)
	 (*ppstring '|>| 1)
	 (*ppend))

	((eq (car x) 'storer!)
	 (*ppbegin 1)
	 (*ppstring '|<| 1)
	 (*ppblank nil 1 0)
	 (ppform (cadr x) 0)
	 (*ppblank nil 1 0)
	 (*ppstring '|,| 1)
	 (*ppblank nil 0 0)
	 (ppform (caddr x) 0)
	 (*ppblank nil 1 0)
	 (*ppstring '|,| 1)
	 (*ppblank nil 0 0)
	 (ppform (cadddr x) 0)
	 (*ppblank nil 2 0)
	 (*ppstring '|>| 1)
	 (*ppend))
	
	((eq (car x) 'selecta!)
         (ppform (cadr x) 0)
	 (*ppstring '|[| 1)
	 (ppform (caddr x) 0)
	 (*ppstring '|]| 1))

	((eq 'selectr! (car x)) 
         (ppform (cadr x) 0)
	 (*ppstring '|.| 1)
	 (ppform (cddr x) 0)
	 (*ppblank nil 0 0))

	((eq 'true! (car x)) (*ppstring 'true 4))

	((eq 'false! (car x)) (*ppstring 'false 5))

	((null (cdr x)) (*ppstring (car x) (flatc (car x))))

	(t 
	 (ppform (car x) p)
	 (*ppbegin 1)
	 (*ppstring '|(| 1)
		    (cond ((cdr x) 
			   (ppform (cadr x) 0)
			   (pplist (cddr x))))
		    (*ppstring '|)| 1)
	 (*ppend)))
  (*ppend))

;;; print a list of the form   + xxx + xxx + xxx + xxx + .....
(defun ppnary (op expr kind val len)
  (cond ((null expr))
	((and (not (atomp (car expr)))
	      (eq (caar expr) op) (get op 'asso))
	 (ppnary op (append (cdar expr) (cdr expr)) kind val len))
	((cdr expr)
	 (ppform (car expr) 
		 (+ (cond ((get op 'asso) 0)(t 1)) (get op 'p)))
	 (*ppblank kind val len)
	 (*ppstring (get op 'pval) (get op 'plen))
	 (*ppblank nil val len)
	 (ppnary op (cdr expr) kind val len))
	(t (ppform (car expr) 
		   (+ (cond ((get op 'asso) 0)(t 1)) (get op 'p))))))


;;; print a list of the form   , xxx, xxx, xxx, xxx, .....
(defun pplist (expr)
       (cond ((null expr))
             (t 
              (*ppstring '|,| 1)
              (*ppblank nil 0 0)                ; break here if necessary
              (ppform (car expr) 0)
              (pplist (cdr expr)))))


