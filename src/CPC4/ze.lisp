
"@(#)ze.l	2.9"

;;;(declare (load 'need.o)
;;;	 (load 'defmac.o)
;;;	 (load 'enode.o)
;;;	 (load 'debug.o)
;;;	 (load 'map.o)
;;;	 (load 'princ.o))

(declarespecial
	  enumlist
	  falsenode
	  truenode
	  zconterm
	  integertype
	  zeronode
	  zfunctionnodes
	  zgenode
	  zgtnode
	  zminusoneterm
	  zmultnode
	  znegterms
	  zonenode
	  zoneterm
	  zposterms
	  zsize
	  zsymbols
	  zsymge
	  zsymgt
	  zsymle
	  zsymlt
	  zsymminus
	  zsymmult
	  zsymplus
	  zterms)

;;;(needs-macros)

(defun normalizelt (l) (normalize (list zsymgt (caddr l) (cadr l))))

(defun normalizele (l) (normalize (list zsymge (caddr l) (cadr l))))

(defun internge (node) (mapc 'findznode (cdr (esuccessors node))))

(defun interngt (node) (mapc 'findznode (cdr (esuccessors node))))

(defun internplus (node)
  (prog (l znode)
	(and (zfield node) (return nil))
	(setq l (makezlist (mapcar 'findznode (cdr (esuccessors node)))))
	(issingleterm node l)
	(xzfield node (list l node nil))
	(or (setq znode (tellz l node)) (return t))
	(or (eq node (getenode znode)) (zmerge node (getenode znode)))))

(defun internminus (node)
  (prog (l znode)
	(and (zfield node) (return nil))
	(setq l (mapcar 'findznode (cdr (esuccessors node))))
	(setq l (cond ((cddr (esuccessors node))
		       (makezlist (list (car l) (znegate (cadr l)))))
		      (t (makezlist (list (znegate (car l)))))))
	(issingleterm node l)
	(xzfield node (list l node nil))
	(or (setq znode (tellz l node)) (return t))
	(or (eq node (getenode znode)) (zmerge node (getenode znode)))))

(defun interntimes (node)
  (prog (x y l znode)
	(and (zfield node) (return nil))
	(setq x (findznode (cadr (esuccessors node))))
	(setq y (findznode (caddr (esuccessors node))))
	(setq l (cond ((zconstant x)
		       (cond ((zconstant y)
			      (list (list (cons (* (zconstant x) (zconstant y))
						1)
					  zonenode)))
			     ((it2 (zconstant x) y))))
		      ((zconstant y) (it2 (zconstant y) x))
		      (t (list (list '(1 . 1) node)))))
        (issingleterm node l)
	(xzfield node (list l node nil))
	(or (setq znode (tellz l node)) (return t))
	(or (eq node (getenode znode)) (zmerge node (getenode znode)))))

(declarespecial singlevar) ; used to communicate between issingleterm and
		              ; issingleterm1

(defun issingleterm (node l)
  (prog (singlevar)
	(mapc 'issingleterm1 l)
	(cond ((null singlevar) (zmerge node zeronode))
	      ((eq singlevar t))
	      (t (zmerge node singlevar)))))

(defun issingleterm1 (pair)
  (cond ((= (caar pair) 0))
	((eq (eroot (cadr pair)) (eroot zeronode)))
	((= (caar pair) (cdar pair)) 
	 (cond (singlevar (setq singlevar t))
	       (t (setq singlevar (cadr pair)))))
	(t (setq singlevar t))))

(defun zmerge (node1 node2)
       (simpdebug (princ-tab '"zmerge: "))
       (simpdebug (princ-tab (enumber node1)))
       (simpdebug (princ-terpri (enumber node2)))
       (emerge node1 node2))
;;;============================================================================
(defun enodenumber (f) 
       (prog (l node znode)
             (setq l (assoc f enumlist))
             (and l (return (cdr l)))
             (setq node (newenode))
             (setq enumlist (cons (cons f node) enumlist))
             (xesuccessors node f)
	     (zsetdatatype node (list 'subrange f f))	; set type of node
             (setq l (list (list (cons f 1) zonenode)))
             (xzfield node (list l node nil))
             (setq znode (tellz l node))
             (or (null znode) (eq (getenode znode) node) (emerge (getenode znode) node))
             (return node)))

(defun findznode (node)
       (cond ((zfield (eroot node)) (zterm (eroot node)))
             (t (makeznode (eroot node)))))

(defun makeznode (node)
       (prog (l znode)
             (setq l (list (list '(1 . 1) node)))
             (xzfield node (list l node nil))
             (setq znode (tellz l node))
             (or (null znode) (eq znode node) (break makeznode))
             (return l)))

(defun makezlist (l)
       (prog (zterms)
             (mapc 'makezlist0 l)
             (return (mapcar 'makezlist2 zterms))))

(defun makezlist0 (x)
       (cond (x (mapc 'makezlist1 x)) 
             (t (princ-terpri '"**ERROR** Unexpected undeclared numeric variable")
                (recover))))

(defun makezlist1 (x)
       (prog (pair)
             (setq pair (assq (cadr x) zterms))
             (cond (pair (rplacd pair (+ (cdr pair) (caar x))))
                   (t (setq zterms (cons (cons (cadr x) (caar x)) zterms))))))

(defun makezlist2 (x) (list (cons (cdr x) 1) (car x)))

;;;============================================================================
(defunobj zunsplice (node) (xzfield node nil)) 

(defun emergez (node z1 z2) 
       (cond ((null z2))
             ((null z1) (pushcontext (cons zunsplice node)) (xzfield node z2)) 
             ((iszpredicate (cadr z1)))
             ((iszpredicate (cadr z2)))
             ((new-eassertz (append (car z1) (znegate (car z2)))))))

(defun iszpredicate (node)
       (and (not (atomp (esuccessors node)))
            (eq zgenode (car (esuccessors node)))))

(defun remnumnode (node) 
       (xzfield node nil)
       (cond ((eq (cdar enumlist) node) (setq enumlist (cdr enumlist)))
             ((assq node enumlist) (break remnumnode))))

(defunobj zgetrue (node matchlist lab pattern) 
       (new-iassertz (makezlist (list (findzterm (cdr (assq 'x matchlist)))
                                      (znegate (findzterm (cdr (assq 'y matchlist))))))))

(defunobj zgefalse (node matchlist lab pattern) 
       (new-iassertz (makezlist (list zminusoneterm
                                      (findzterm (cdr (assq 'y matchlist)))
                                      (znegate (findzterm (cdr (assq 'x matchlist))))))))

(defunobj zgttrue (node matchlist lab pattern) 
       (new-iassertz (makezlist (list zminusoneterm
                                      (findzterm (cdr (assq 'x matchlist)))
                                      (znegate (findzterm (cdr (assq 'y matchlist))))))))

(defunobj zgtfalse (node matchlist lab pattern) 
       (new-iassertz (makezlist (list (findzterm (cdr (assq 'y matchlist)))
                                      (znegate (findzterm (cdr (assq 'x matchlist))))))))

(defun new-iassertz (l) 
       (simpdebug (terpri))
       (simpdebug (princ-tab '"iassertz: "))
       (simpdebug (mapcone (princ-tab (zprint x)) l))
       (simpdebug (terpri))
       (iassertz l) 
       (and (emptyz) (propagatefalse))) 

(defun new-eassertz (l) 
       (simpdebug (terpri))
       (simpdebug (princ-tab '"eassertz: ") )
       (simpdebug (mapcone (princ-tab (zprint x)) l))
       (simpdebug (terpri))
       (eassertz l) 
       (and (emptyz) (propagatefalse))) 

(defun propeq (node1 node2)
       (simpdebug (princ-tab '"propeq: "))
       (simpdebug (princ-tab (enumber node1)))
       (simpdebug (princ-terpri (enumber node2)))
       (emerge node1 node2)) 

(defun znegate (l) (mapcar 'im2 l)) 
;;;============================================================================
;;; ppzterm

(defun ppzterm (l) 
       (prog (zterms znegterms zposterms zconterm zsize) 
             (setq l (zterm l))
             (setq zconterm 0)
             (setq zsize 0)
             (mapc 'ppzterm3 l)
             (mapc 'ppzterm4 zterms)
             (and (> zsize 200000) (return '(200000 . printerror)))
             (cond ((< zconterm 0) (setq znegterms (cons (- zconterm) znegterms)))
                   ((> zconterm 0) (setq zposterms (cons zconterm zposterms))))
             (setq zsize (+ zsize (length znegterms) (length zposterms)))
             (setq zterms (ppzterm5 zsymminus znegterms (ppzterm5 zsymplus zposterms 0)))
             (return (cons zsize zterms)))) 

(defun ppzge (node1 node2 isgt)
       (prog (zterms znegterms zposterms zconterm zsize) 
             (setq zconterm 0)
             (setq zsize 0)
;            (or (and (zterm (eroot node1)) (zterm (eroot node2))) (break ppzge))
             (mapc 'ppzterm3 (zterm (eroot node1)))
             (mapc 'ppzterm3 (znegate (zterm (eroot node2))))
             (and isgt (ppzterm3 (car zminusoneterm)))
             (mapc 'ppzterm4 zterms)
             (and (> zsize 200000) (return '(200000 . printerror)))
             (setq zterms (cond ((= zconterm 0)
                                 (list zsymle
                                       (ppzterm5 zsymplus znegterms 0)
                                       (ppzterm5 zsymplus zposterms 0)))
                                ((= zconterm -1)
                                 (list zsymlt
                                       (ppzterm5 zsymplus znegterms 0)
                                       (ppzterm5 zsymplus zposterms 0)))
                                ((> zconterm 0)
                                 (list zsymle
                                       (ppzterm5 zsymplus znegterms 0)
                                       (ppzterm5 zsymplus (cons zconterm zposterms) 0)))
                                (t (list zsymle
                                         (ppzterm5 zsymplus
                                                   (cons (- zconterm) znegterms)
                                                   0)
                                         (ppzterm5 zsymplus zposterms 0)))))
             (setq zsize (+ zsize (length znegterms) (length zposterms)))
             (return (cons zsize zterms)))) 

(defun zconstant (l) 
       (prog (temp) 
             (or l (return 0))
             (or (eq (cadar l) zonenode) (return nil))
             (setq temp (zconstant (cdr l)))
             (or temp (return nil))
             (return (+ temp (caaar l))))) 

(defun im2 (pair) (cons (cons (- (caar pair)) (cdar pair)) (cdr pair))) 

(defun it2 (n zl) (cond (zl (cons (it3 n (car zl)) (it2 n (cdr zl)))))) 

(defun it3 (n pair) (cons (cons (* n (caar pair)) (cdar pair)) (cdr pair))) 
;;;============================================================================
;;; ppzterm 2

(defun ppzterm3 (x) 
       (prog (root pair) 
             (setq root (eroot (cadr x)))
             (setq pair (assq root zterms))
             (cond (pair (rplacd pair (+ (cdr pair) (caar x))))
                   (t (setq zterms (cons (cons root (caar x)) zterms)))))) 

(defun ppzterm4 (pair) 
       (prog (f size coeff) 
             (setq coeff (cdr pair))
             (and (= 0 coeff) (return nil))
             (setq f (eform2 (car pair)))
             (setq size (car f))
             (setq f (cdr f))
             (and (numberp f) (return (setq zconterm (+ zconterm (* f coeff)))))
             (setq zsize (+ zsize size))
             (cond ((= coeff 1) (setq zposterms (cons f zposterms)))
                   ((> coeff 1)
                    (setq zposterms (cons (list zsymmult coeff f) zposterms)))
                   ((= coeff -1) (setq znegterms (cons f znegterms)))
                   (t (setq znegterms (cons (list zsymmult (- coeff) f) znegterms)))))) 

(defun ppzterm5 (op terms result) 
       (cond (terms (cond ((equal result 0)
                           (cond ((eq op zsymplus) (ppzterm5 op (cdr terms) (car terms)))
                                 (t (ppzterm5 op
                                              (cdr terms)
                                              (cond ((numberp (car terms))
                                                     (- (car terms)))
                                                    ((list zsymminus (car terms))))))))
                          (t (ppzterm5 op (cdr terms) (list op result (car terms))))))
             (result))) 

(defun ppzmult (a b) 
       (setq a (eform2 (eroot a)))
       (setq b (eform2 (eroot b)))
       (cond ((numberp (cdr a)) 
              (cond ((numberp (cdr b)) (cons 1 (* (cdr a) (cdr b))))
                    (t (ppzmult1 (cdr a) (cdr b) (car b)))))
             ((numberp (cdr b)) (ppzmult1 (cdr b) (cdr a) (car a)))
             (t (cons (+ (car a) (car b) 1) (list zsymmult (cdr a) (cdr b))))))

(defun ppzmult1 (coeff var size)
       (cond ((= coeff 0) (cons 1 0))
             ((= coeff 1) (cons size var))
             ((= coeff -1) (cons (1+ size) (list zsymminus var)))
             (t (cons (+ 2 size) (list zsymmult coeff var)))))

;
;	Type checking
;
;
;	zetypechk  --  returns list of all non-numeric args out of those
;		       given.   The node should be a function call node.
;

(defun zetypechk (node)
  (cond ((null (esuccessors node)) nil) ; if no args, no problem
	((atomp (esuccessors node)) (internalerror "Atom in successor list"))
	((null (cdr (esuccessors node))) ; 1 arg case, must be variable
	 (zetypechk1 (esuccessors node))) ; check first arg
	(t (zetypechk1 (cdr (esuccessors node)))))); check all args to fn

(defun zetypechk1 (lst)
    (cond ((null lst) nil)		; if end of list, OK
	  ((not (isenode (car lst))) (internalerror "Non-node in successors"))
	  ((equal (car (getdatatype (car lst))) 'subrange)
		(zetypechk1 (cdr lst)))		; if numeric, recurse
	  ((equal (car (getdatatype (car lst))) 'integer)
		(zetypechk1 (cdr lst)))		; if numeric, recurse
	  (t (cons (car lst) (zetypechk1 (cdr lst)))) ; add to list if not
	))
	
;;;============================================================================
;;; initialization

(defun definearithmetic (l) (setq zsymbols l))

(defun zeinit ()
       (prog (l)
             (zintern zsymbols 
                      '(internplus internminus interntimes internge interngt internle internlt)
                      '(zsymplus zsymminus zsymmult zsymge zsymgt zsymle zsymlt))
             (setq zfunctionnodes (list (enode zsymplus) (enode zsymminus)))
             (setq zmultnode (enode zsymmult))
             (setq zgenode (enode zsymge))
             (setq zgtnode (enode zsymgt))
             (makenodedemon truenode (list zsymge 'x 'y) zgetrue '(x y))
             (makenodedemon falsenode (list zsymge 'x 'y) zgefalse '(x y))
             (makenodedemon truenode (list zsymgt 'x 'y) zgttrue '(x y))
             (makenodedemon falsenode (list zsymgt 'x 'y) zgtfalse '(x y))
             (putprop zsymle 'normalizele 'normalize)
             (putprop zsymlt 'normalizelt 'normalize)
             (setq zeronode (newenode))
             (setq zonenode (newenode))
             (setq enumlist (list (cons 0 zeronode) (cons 1 zonenode)))
             (xesuccessors zonenode 1)
             (setq zoneterm (list '(1 . 1) zonenode))
             (xzfield zonenode (list (list zoneterm) zonenode (makeonez zonenode)))
             (xesuccessors zeronode 0)
             (xzfield zeronode (list (list (list '(0 . 1) zonenode)) 
                                     zeronode (makezeroz zeronode)))
             (tellz (list zoneterm) zonenode)
             (setq zminusoneterm (list (list '(-1 . 1) zonenode)))
	     (zsetdatatype zeronode '(subrange 0 0))	; 0 is integer
	     (zsetdatatype zonenode '(subrange 1 1))	; 1 is integer
             (return t)))

;
;	zintern  --  mark symbol so that E will know when to call Z
;
(defun zintern (l1 l2 l3)
       (cond (l1
              (and (car l1) (putprop (car l1) (car l2) 'intern))
              (and (car l1) (putprop (car l1) 'zetypechk 'interntypechk))
              (set (car l3) (car l1))
              (zintern (cdr l1) (cdr l2) (cdr l3)))))

