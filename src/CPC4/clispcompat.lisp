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
(defmacro defprop (symb newval indicator)
   `(setf (get ',symb ',indicator) ',newval))
 
;;;
;;; putprop - add to property list, evaluting params
;;;
(defun putprop (symb newval indicator)
      (setf (get  symb indicator) newval))

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
;;; explodec -- symbol name to list of chars
;;;
;;; ***CHECK THIS*** for MacLisp compatiblity.
;;;
(defun explodec (s)
      (map 'list #'(lambda (c) c) (string s)))
      