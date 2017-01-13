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
