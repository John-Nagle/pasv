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
