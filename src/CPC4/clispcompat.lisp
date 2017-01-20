;;;
;;; clispcompat.l  -- compatibiilty for Common LISP, GNU clist version
;;;
;;; John Nagle  January, 2017
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
;;; multiply -- product of two numbers
;;;
;;; Was "times" in Franz LISP, but that has a different meaning in CL.
;;;
(defun multiply (x y) (* x y))
;;;
;;; ncons -- equivalent to (cons arg nil)
;;;
(defun ncons (x) (cons x nil))
;;;
;;; defprop -- add to property list
;;;
(defmacro defprop (symbol newval indicator)
   `(setf (get ',symbol ',indicator) ',newval))

    
;;;
;;; defsmac1 - Common LISP version based on Franz Lisp version in defmac.
;;;
;;; ***NOT CORRECT***
;;; ***CDR of APP probably off by one due to CL/Franz macro arg differences***
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)  ; because this macro evaluates functions

    (defmacro defsmacbad (&body args)
       `(defsmac1 ',(car args) ',(cadr args) ',(cddr args))) ;;; ***args references seem to be correct

    (defun defsmac1 (name formals body)
           `(defmacro ,name (&body app) 
                         ,(defsmac2 formals
                                      (cond ((cdr body) (cons 'progn body)) 
                                            (t (car body))))))

    (defun defsmac2 (formals body)
           `(sublis  ,(defsmac3 formals 1) (quote ,body)))

    (defun defsmac3 (formals n)
           (cond ((null formals) nil)
                 (`(cons (cons (quote ,(car formals)) (car ,(defsmac4 n)))
                               ,(defsmac3 (cdr formals) (1+ n))))))

    (defun defsmac4 (n) (cond ((= n 0) 'app) ((list 'cdr (defsmac4 (1- n)))))) ;;; ***'app needs first elt of list removed***
)

;;;
;;; New approach from HN. doesn't work if expr has more than one element.
;;;
;;; Tested OK on "princ-terpri".  Accepts multiple params to 'expr'
(defmacro defsmac (name params &rest expr)
      `(defmacro ,name ,params
         `,(cons 'progn (sublis (mapcar 'cons ',params (list ,@params))  ', expr))))
    
;;; ***NEEDS WORK - untested*** Only used at one place at "z.lisp"
(defmacro defmac (name params expr)
      `(defmacro ,name ,params
         (list '(lambda ,params ,expr) ,@params)))
