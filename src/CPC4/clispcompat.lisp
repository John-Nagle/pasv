;;;
;;; clispcompat.l  -- compatibiilty for Common LISP, GNU clist version
;;;
;;; John Nagle  January, 2017
;;;
;;; Global variables
;;;
(defvar errport *error-output*)         ; error messages to stderr
(defvar poport *standard-output*)   ; other output to stdout
;;;
;;;
;;; Functions and macros
;;;
;;;
;;; comment - does nothing
;;;
(defmacro comment (&rest args))

;;;
;;; declarespecial -- declares special variables at top level
;;;
(defmacro declarespecial (&rest args)
    `(proclaim '(special ,@args)))
    
;;;
;;; loadobjectfiles  -- load object files needed for separate
;;;     compilation.  Does nothing, because separate 
;;;     compilation is no longer being used.  Better to
;;;     keep the dependency info than lose it, though.
;;;
(defmacro loadobjectfiles (&rest args))

;;;
;;; oldstylearray - Franz Lisp style arrays.
;;;
;;; Create array: (oldstylearray arrayname elttype length)
;;; Access array: (arrayname index)
;;; Store into array:  (store (arrayname index) newval)
;;;
;;; Limited to one dimension.
;;;
(defmacro oldstylearray (arrayname elttype length)
  `(progn
        (setq ,arrayname (make-array ,length :element-type ,elttype)) ; create array
        (defun ,arrayname (n) (aref ,arrayname n))))  ; access via (arrname index)

(defmacro store (arrayref newval) 
    `(setf  (aref ,(car arrayref) ,(cadr arrayref)) ,newval))

;;;
;;; add1  -- add 1 to number
;;;
(defun add1 (n) (+ 1 n))
;;;
;;; multiply -- product of two numbers
;;;
;;; Was "times" in Franz LISP, but that has a different meaning in CL.
;;;
(defun multiply (x y) (* x y))
;;;
;;; difference  -- n-ary subtraction
;;;
(defmacro difference (&rest args) `(- ,@args))
;;;
;;; quotient -- division
;;;
;;; NOTE - in Common LISP, this can produce a rational number.
;;; Unclear if that's OK. May need to trap that for debug.
;;;
(defun quotient (a b) (/ a b))
;;;
;;; lessp -- numeric comparison
;;;
(defun lessp (a b) (< a b))
;;;
;;; ncons -- equivalent to (cons arg nil)
;;;
(defun ncons (x) (cons x nil))
;;;
;;; defprop -- add to property list
;;;
(defmacro defprop (symb newval indicator)
   `(setf (get ',symb ',indicator) ',newval))
 
;;;
;;; putprop - add to property list, evaluting params
;;;
(defun putprop (symb newval indicator)
      (setf (get  symb indicator) newval))
;;;
;;; append1  
;;;
;;; "This is equivalent to (append 'l_arg1 (list 'g_arg2)" - Franz LISP manual
;;;
(defun append1 (arg1 arg2)
      (append arg1 (list arg2)))

;;;
;;; defsmac/defmac -- see defmac.lisp for original MacLISP code and comments.
;;;
;;; New approach from "pdw" on Hacker News.
;;; Tested OK on "princ-terpri".  Accepts multiple params to 'expr'
;;;
(defmacro defsmac (name params &rest expr)
      `(defmacro ,name ,params
         `,(cons 'progn (sublis (mapcar 'cons ',params (list ,@params))  ', expr))))
    
;;; ***NEEDS WORK - untested*** Only used at one place at "z.lisp".
(defmacro defmac (name params expr)
      `(defmacro ,name ,params
         (list '(lambda ,params ,expr) ,@params)))
         
;;;
;;; concat -- concatenate two symbols
;;;
(defun concat (a b)                                 ; MacLISP compatibility
   (values (intern 
       (string-upcase (concatenate 'string 
            (string a) 
            (string b))))))
            
;;;
;;; explode, explodec -- symbol name to list of chars
;;;
;;; ***CHECK THIS*** for MacLisp compatiblity.
;;;
(defun explodec (s)
      (map 'list #'(lambda (c) c) (princ-to-string s)))
(defun explode (s)
      (map 'list #'(lambda (c) c) (prin1-to-string s)))
      
;;;
;;; oblist -- return a list of all objects in the current package.
;;;
;;; This is used only to clean up properties on symbols.
;;;
(defun oblist ()
      (let ((allsyms nil))
            (do-symbols (x *PACKAGE*) (setq allsyms (cons x allsyms)))
            allsyms))
            
;;;
;;; memq -- search list, return tail on find. Test with "eq"
;;;
(defun memq (key lst )
      (member key lst :test #'eq))
;;;
;;; patom -- print without quotes
;;;
;;; ***TEMP*** may need special char removal
;;;
(defun patom (item &optional (port nil))
      (princ item port))  

;;;
;;; flatc -- the number of characters required to print g_form using patom
;;;
(defun flatc (s) :
      (length (princ-to-string s)))

;;;
;;; implode -- per Barry Margolin post in 1987.
;;;
(defun implode (charlist)
  (intern (map 'string #'character charlist)))
  
;;;
;;;     ptime - CPU time used
;;;
;;;     Returns (cpu time, GC time).
;;;     Currently GC time is zero.
;;;
(defun ptime ()
      (list (get-internal-real-time) 0))
      