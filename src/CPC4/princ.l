"@(#)princ.l	2.1"


;;;(declare (macros t) (load 'need.o) (load 'defmac.o))
(needs-macros)

;;; (echoterpri) prints a (terpri) on any open file.
;;; It is useful for echoing to backup files carriage-returns typed at
;;; terminals.

(defsmac echoterpri
         nil
         (cond (^q (terpri)) (^r ((lambda (^w) (terpri)) t)))) 

(defsmac princ-start (x) (echoterpri) (princ x)) 

(defsmac princ-start-terpri (x) (echoterpri) (princ-terpri x)) 

(defsmac princ-tab (x) (princ x) (princ '|	|))

(defsmac princ-terpri (x) (princ x) (terpri))
