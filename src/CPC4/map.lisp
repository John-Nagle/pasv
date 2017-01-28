"@(#)map.l	2.1"

;;;============================================================================
;;;  mapping macros 

;;;(declare (macros t) (load 'need.o) (load 'defmac.o))
;;;(needs-macros)

;;;
;;; (mapcone (foo x) l) expands into (mapc '(lambda(x) (foo x)) l)
;;; Thus it calls (foo x) on every element x of l.

(defsmac mapcone (f l) (mapc (function (lambda (x) f)) l)) 

;;; (mapone (foo x) l) expands into (map '(lambda(x) (foo x)) l)
(defsmac mapone (f l) (map 'list (function (lambda (x) f)) l)) ; ***NOT SURE ABOUT THIS***

;;; (mapctwo (foo x1 x2) l1 l2) calls (foo x1 x2) on every pair of elements
;;; x1 from l1, and x2 from l2.   i.e. it does an n-squared mapc.

(defsmac mapctwo (f l1 l2)
         (and l2 (mapc (function (lambda (x1)
	     (mapc (function (lambda (x2) f)) l2))) l1)))
