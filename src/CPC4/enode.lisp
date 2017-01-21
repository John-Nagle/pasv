"@(#)enode.l	2.5"

;;; This file contains macros used to define and manipulate enodes

;;;(declare (macros t)
;;;	 (load 'need.o)
;;;	 (load 'defmac.o)
;;;	 (load 'hunkshell.o))

;;; (needs-macros)

;;; An enode is a node in a graph that represents an equivalence
;;; class of forms.  Enodes are the fundamental data structure
;;; used by the simplifier.

(hunkshell enode
    eroot           ; The representative node of the equivalence class that
		    ;    contains this node

    eqclass         ; Field used to circularly link all the nodes of an
		    ;    equivalence class

    esuccessors     ; If this field is an atom, this node represents that
		    ;    atom.  If this field is a list, the list contains
		    ;    enodes reprenenting operands in a expression.

    eslength        ; If esuccessors is an atom, this field is nil.  If
		    ;    esuccessors is a list, this field is the length
		    ;    of that list.

    epredecessors   ; A list containing every enode that has this node
		    ;    as a successor.  There are no duplicates in
		    ;    predecessor lists.

    econgruent      ; If two or more nodes are congruent, all but one
		    ;    has t in its congruent field.

    edemon          ; List of partial matches (nodedemons) waiting for
		    ;    this enode to be merged.

    emergedemon     ; A list of dotten pairs of the form (n . f), where n
		    ;    is an enode and f is a function.  f is called when
		    ;    this enode becomes equivalent to n.

    epattern        ; List of patterns whose leftmost symbol is the print
		    ;    name of this atomic node.

    zfield          ; If non-nil, this equivalence class represents an
		    ;     arithmetic term.

    eqlength        ; Size of equivalence class containing this node.  Only
		    ;     valid in root nodes.

    enumber         ; A unique integer used for diagnostic purposes.

    eavail          ; All allocated enodes are linked through this field.

    eheight         ; Used to control pattern matching.

    etype           ; Individual provers may store information here.

    eprinttag       ; Controls printing.

    edatatype	    ; Type of data in node

    etypewait	    ; Queue of transactions waiting for type change
    )

;
;	Utility functions for enodes
;
(defsmac zterm (node) (car (zfield node))) 

(defsmac findzterm (node)
         (and (zfield (eroot node)) (zterm (eroot node)))) 

(defmac isenode (x) (and (hunkp x) (= (hunksize x) 18))) 

;;;============================================================================
;;; mapeqclass is used to cycle through an equivalence class and
;;; do something to every member of the class.  f should be an expression
;;; with 'x' as a free variable.  For every enode in the class containing
;;; l, x is lambda-bound to that enode and f is evaluated.  The macro then
;;; evaulates r and returns the value.  (When r is evaluated, x is bound to l.))

(defsmac mapeqclass
         (f r l)
         (prog (x finish) 
               (setq finish l)
               (setq x finish)
          a    f
	       (setq x (eqclass x))
               (and (eq x finish) (return r))
               (go a))) 

;;; mapclass is similar to mapeqclass.  The difference is that instead
;;; of the entire class, only the portion of a class from start to finish
;;; has f applied to it.  The argument r is discarded (not even evaluated).

(defsmac mapclass
         (f r start finish)
         (prog (x finishx) 
               (setq x start)
               (setq finishx finish)
         a     f
	       (and (eq x finishx) (return nil))
               (setq x (eqclass x))
               (go a))) 

