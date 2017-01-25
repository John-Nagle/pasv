"@(#)z.l	2.4"

;;;declare (load 'need.o)
;;;	 (load 'defmac.o)
;;;	 (load 'hunkshell.o)
;;;	 (load 'enode.o)
;;;	 (load 'debug.o)
;;;	 (load 'princ.o)

	 (declarespecial maxrowz* lastrowz* savepivotz* drowz* historyz* onecolz*
		  maxcolz* zonenode zeronode dcolz* didpivotz* lastcolz*
		  availz* floorz* zmarkz*)
;;;(needs-macros)

;;; The maclisp compiler used the following declaration, which is not
;;; used in Franz lisp.
;;;      (fixnum lastrowz* lastcolz* maxrowz* maxcolz* drowz* dcolz* onecolz*)
;;;      (fixnum (crossingparamz* notype notype))

;   Define structure of a znode.
(hunkshell node num den up left row col prehistory pushcount)

;;; ***WRONG*** this is not correct in CL. 
;;; MacLISP and Franz LISP allowed accessing hunks with car/cdr/cxr.
;;; Which field is which seems to be different in MacLISP and Franz LISP.
;;; This seems to be code intended to work on either a list or a hunk.
;;; But which field is "caddr l"?
;;; 
(defun getznode (l) (caddr l)) ; Access to field 3 of hunk?. Not allowed in CL

;;;;(defun getenode (l) (cadr l)) ; Original version. Access to field 2 (?) of hunk. Not allowed in CL
;;;;(defun getenode (l) (den l)) ;;; CL version ***TEMP*** very unsure about this
;;; Attempt to replicate MacLISP semantics
;;; Code applies this to znodes.
;;;;(defun getenode (l)
;;;;       (cond    ((isznode l) (up l))        ; if znode, get second field? third field? Huh?
;;;;                    ((t (cadr l)))))                 ; otherwise, original cadr
;   Debug version - unclear if this will work.
(defun getenode (l)
       (let ((en (cond ((isznode l) (num l))    ; if znode, use num (not sure about this)
                            ((t (cadr l))))))                 ; if list, use cadr
                     (unless (isenode en) (internalerror "getenode - non-enode returned")) ; must return an enode
                     en))

;;;;(defun isznode (x) (and (hunkp x) (= (hunksize x) 8))) 
(defun isznode (x) (equal (type-of x) 'node))   ; CL

(defsmac ownerz* (p) (num p)) 

(defsmac xownerz* (p new) (xnum p new)) 

(defun nodeownedbyz* (var) (getznode (zfield var)))

(defun makeownerz* (p var) 
       (xownerz* p var)
       (or (getznode (zfield var)) (rplaca (cddr (zfield var)) p)))

(defun disownz* (var) 
       (and (isenode var) (zfield var) (rplaca (cddr (zfield var)) nil)))

(defun makeonez (node) 
       (xownerz* (basecolz* onecolz*) node)
       (basecolz* onecolz*))

(defun makezeroz (node) 
       (xownerz* floorz* node)
       floorz*)

(defun pushz nil (xpushcount historyz* (1+ (pushcount historyz*))) nil) 

(defun popz nil 
       (do nil
           ((> (pushcount historyz*) 0)
            (xpushcount historyz* (1- (pushcount historyz*)))
            nil)
           (and (= (col historyz*) onecolz*) (return t))
           (cond ((= (col historyz*) 0) (deleterowz* (row historyz*)))
                 (t (or (> (col historyz*) dcolz*) (setq dcolz* (1- (col historyz*))))
                    (deletecolz* (col historyz*)))))) 

(defun emptyz nil (and (> lastrowz* drowz*) (> (onesignz* lastrowz*) 0))) 
;;;============================================================================
(defun tellz (term node)
       (prog (var)
             (simpdebug (terpri))
             (simpdebug (princ-tab '"tellz: ") )
             (simpdebug (mapcone (princ-tab (zprint x)) term))
             (simpdebug (princ-tab '"node: "))
             (simpdebug (princ-terpri (enumber node)))
             (addrowz* term)
             (ar2z* 1 1 zeronode)
             (setq var (rowisjustvarz* lastrowz*))
             (cond ((and var (isenode var)) 
                    (deleterowz* lastrowz*) 
                    (return (getznode (zfield var)))))
             (return (do ((i (1- lastrowz*) (1- i)))
                         ((= i 0)
                          (makeownerz* (baserowz* lastrowz*) node)
                          (setq drowz* (1+ drowz*))
                          (swaprowz* lastrowz* drowz*)
                          (baserowz* drowz*))
                         (and (eqrowz* (baserowz* i) (baserowz* lastrowz*) nil)
                              (progn (deleterowz* lastrowz*)
                                     (return (baserowz* i))))))))

(defun iassertz (term) 
       (prog (piv) 
             (and (emptyz) (return nil))
             (or term (return nil))
             (addrowz* term)
             (and (subsumedz*) (return nil))
        l    (and (< (onesignz* lastrowz*) 0) (return nil))
             (setq piv (findpivotz* lastrowz*))
             (and piv (= (row piv) lastrowz*) (onesigniszeroz* lastrowz*) (return nil))
             (and piv (progn (pivotz* piv) (go l)))
             (and (emptyz) (return nil))
             (setq piv (anypivotz* lastrowz*))
             (pivotz* piv)
             (zerocolz* piv))) 

(defun eassertz (term) 
       (prog (piv) 
             (and (emptyz) (return nil))
             (or term (return nil))
             (addrowz* term)
             (cond ((onesigniszeroz* lastrowz*)
                    (setq piv (anypivotz* lastrowz*))
                    (or piv (progn (deleterowz* lastrowz*) (return nil)))
                    (pivotz* piv)
                    (zerocolz* piv)
                    (return nil)))
             (and (< (onesignz* lastrowz*) 0) (negaterowz* lastrowz*))
        l    (setq piv (findpivotz* lastrowz*))
             (or piv (return nil))
             (and (or (= (row piv) lastrowz*)
                      (not (lessp (crossingparamz* piv lastrowz*) 0)))
                  (progn (pivotz* savepivotz*) (zerocolz* savepivotz*) (return nil)))
             (pivotz* piv)
             (go l))) 

;;;============================================================================

(defun vgcd (m n) (gcd1 (abs m) (abs n)))

(defun gcd1 (m n)
       (cond ((zerop n) m)
             (t (vgcd n (mod m n)))))
      
(defun subsumedz* nil 
       (prog (piv) 
             (and (> (onesignz* lastrowz*) 0) (return nil))
             (negaterowz* lastrowz*)
        l    (setq piv (findpivotz* lastrowz*))
             (or piv (progn (deleterowz* lastrowz*) (return t)))
             (and (or (= (row piv) lastrowz*) (> (crossingparamz* piv lastrowz*) 0))
                  (progn (negaterowz* lastrowz*) (return nil)))
             (pivotz* piv)
             (go l))) 

(defun zequalities nil 
       (do ((i dcolz* (1- i)) (l))
           ((not (> i 0)) l)
           (setq l (cons (ownerz* (basecolz* i)) l)))) 

(defsmac zc2z* ()
         (catch 'zcatchtag (do ((i lastrowz* (1- i)) (piv) (q)) ; CL - must name throw/catch
                    ((not (> i drowz*)))
                    (setq q (baserowz* i))
                    (and (ownerz* q)
                         (not (isenode (ownerz* q)))
                         (not (eq (car (ownerz* q)) zmarkz*))
                         (onesigniszeroz* i)
                         (anypivotz* i)
                         (prog nil 
                          l3   (setq piv (findpivotz* i))
                               (or piv
                                   (progn (setq p (anypivotz* i)) (pivotz* p) (throw 'zcatchtag t))) ; CL - must name throw/catch
                               (and (onesigniszeroz* (row piv))
                                    (not (= (row piv) i))
                                    (progn (setq didpivotz* t) (pivotz* piv) (go l3)))
                               (markz* q)))))) 

(defun zerocolz* (p) 
       (prog (didpivotz*) 
        l    (killcolz* p)
        l2   (setq didpivotz* nil)
             (and (zc2z*) (go l))
             (and didpivotz* (go l2))
             (unmarkallz*))) 

(defun eqrowz* (x1 x2 flag) 
       (do ((p1 (left x1) (left p1)) (p2 (left x2) (left p2))) (nil)
           (cond ((> (col p1) dcolz*)
                  (or (= (col p1) (col p2)) (return nil))
                  (or (and (equal (num p1) (num p2))
			   (equal (den p1) (den p2)))
		      (return nil)))
                 (t (and (> (col p2) dcolz*) (return nil))
                    (and flag
                         (return (cond ((= (col p1) dcolz*)
                                        (not (and (= (col p2) dcolz*)
                                                  (equal (num p1) (num p2))
                                                  (equal (den p1) (den p2)))))
                                       (t (= (col p2) dcolz*)))))
                    (return t))))) 

(defmac rightz* (j p) (do ((q p (left q))) ((not (> (col (left q)) j)) q))) 

(defmac downz* (i p) (do ((q p (up q))) ((not (> (row (up q)) i)) q))) 
;;;============================================================================
(defun addrowz* (term) 
       (prog (totalnum totalden) 
             (setq totalnum 0)
             (setq totalden 1)
             (newrowz* term)
             (do ((l term (cdr l)) (n1))
                 ((null l))
                 (or (eq (cadar l) zonenode)
                     (progn (setq totalnum (add (multiply totalnum (cdaar l))
                                               (multiply totalden (caaar l))))
                            (setq totalden (multiply totalden (cdaar l)))
                            (setq n1 (vgcd totalnum totalden))
                            (setq totalnum (quotient totalnum n1))
                            (setq totalden (quotient totalden n1))))
                 (ar2z* (caaar l) (cdaar l) (cadar l)))
             (comment in case the uses rats were not cannonical)
             (cond ((lessp totalden 0) (setq totalden (difference totalden)))
                   (t (setq totalnum (difference totalnum))))
             (ar2z* totalnum totalden zeronode)
             (xprehistory (baserowz* lastrowz*) historyz*)
             (setq historyz* (baserowz* lastrowz*)))) 

(defun ar2z* (coefnum coefden var) 
       (prog (p) 
             (and (zerop coefnum) (return nil))
             (comment needed for total even if users term has no zeros)
             (cond ((setq p (nodeownedbyz* var))
                    (cond ((= (col p) 0)
                           (rowopz* p (baserowz* lastrowz*) coefnum coefden))
                          (t (inctableauelemz* (baserowz* lastrowz*)
                                                 p
                                                 (difference coefnum)
                                                 coefden))))
                   (t (newcolz*)
                      (makeownerz* (basecolz* lastcolz*) var)
                      (setq p (nodez* (difference coefnum)
                                      coefden
                                      (basecolz* lastcolz*)
                                      (left (rightz* lastcolz* (baserowz* lastrowz*)))
                                      lastrowz*
                                      lastcolz*
                                      nil
                                      0))
                      (fix0z* p)
                      (xup (basecolz* lastcolz*) p)
                      (xleft (rightz* lastcolz* (baserowz* lastrowz*)) p)
                      nil)))) 

(defun rowisjustvarz* (i) 
       (prog (x) 
             (setq x (baserowz* i))
             (return (and (onesigniszeroz* i)
                          (> (col (left x)) dcolz*)
                          (not (> (col (left (left x))) dcolz*))
                          (equal (num (left x)) -1)
                          (onep (den (left x)))
                          (ownerz* (basecolz* (col (left x)))))))) 

;;;============================================================================
(defun killcolz* (p) 
       (swapcolz* (col p) (1+ dcolz*))
       (setq dcolz* (1+ dcolz*))
       (do ((q (up (basecolz* dcolz*)) (up q)))
           ((= (row q) 0))
           (kc2z* (baserowz* (row q))))) 

(defun kc2z* (x) 
       (prog (var) 
             (or (atom (ownerz* x)) (isenode (ownerz* x)) (return nil))
             (setq var (rowisjustvarz* (row x)))
             (cond ((and (isenode var) (= (col (left (left x))) dcolz*))
                    (propeq var (ownerz* x))))
             (do ((i lastrowz* (1- i)))
                 ((= i 0))
                 (and (not (= i (row x)))
                      (eqrowz* (baserowz* i) x t)
                      (propeq (ownerz* x) (ownerz* (baserowz* i))))))) 

(defun deleterowz* (i) 
       (setq historyz* (prehistory historyz*))
       (cond ((> i drowz*)) (t (swaprowz* i drowz*) (setq i drowz* drowz* (1- drowz*))))
       (swaprowz* i lastrowz*)
       (setq lastrowz* (1- lastrowz*))
       (do ((p (left (baserowz* (1+ lastrowz*))) (left p)))
           ((= (col p) 0) (disownz* (ownerz* p)) (makeavailz* p))
           (xup (basecolz* (col p)) (up p))
           (cond ((and (= (row (up p)) 0)
                       (not (= (col p) onecolz*))
                       (not (eq (up p) floorz*)))
                  (swapcolz* (col p) lastcolz*)
                  (disownz* (ownerz* (basecolz* lastcolz*)))
                  (makeavailz* (basecolz* lastcolz*))
                  (setq lastcolz* (1- lastcolz*))))
           (makeavailz* p))) 

(defun deletecolz* (j) 
       (comment we can assume j > dcolz*)
       (prog (champ champscorenum champscoreden) 
             (setq champscorenum 1)
             (setq champscoreden 0)
             (do ((p (up (basecolz* j)) (up p)) (q) (n1) (n2) (n3))
                 ((= (row p) 0))
                 (setq q (left (baserowz* (row p))))
                 (or (= (col q) onecolz*) (progn (setq champ p) (return nil)))
                 (setq n1 (multiply (den p) (num q)))
                 (setq n2 (multiply (num p) (den q)))
                 (cond ((lessp (abs (multiply n1 champscoreden))
			       (abs (multiply n2 champscorenum)))
                        (setq champ p)
                        (setq n3 (vgcd n1 n2))
                        (setq champscorenum (quotient n1 n3))
                        (setq champscoreden (quotient n2 n3)))))
             (pivotz* champ)
             (deleterowz* (row champ)))) 

;;;============================================================================
(defun findpivotz* (i) 
       (prog (q champ champscorenum champscoreden) 
             (setq q (left (baserowz* i)))
        l    (or (> (col q) dcolz*) (return nil))
             (and (eq (ownerz* (basecolz* (col q))) zonenode) 
                  (setq q (left q)) (go l))
             (and (greaterp (num q) 0) (progn (setq q (left q)) (go l)))
             (setq champscorenum -1)
             (setq champscoreden 0)
             (setq champ q savepivotz* q)
             (do ((p (up (basecolz* (col q))) (up p)))
                 ((not (> (row p) drowz*)))
                 (prog (challengernum challengerden) 
                       (and (lessp (num p) 0) (return nil))
                       (cond ((onesigniszeroz* (row p))
                              (setq challengernum 0)
                              (setq challengerden 1))
                             (t (setq challengernum (num (left (baserowz* (row p)))))
                                (setq challengerden (den (left (baserowz* (row p)))))))
                       (and (greaterp (multiply challengernum 
					     (den p)
					     champscoreden)
                               (multiply challengerden (num p) champscorenum))
                            (prog (temp) 
                                  (setq champscorenum (multiply challengernum
							     (den p)))
                                  (setq champscoreden (multiply challengerden
							     (num p)))
                                  (setq temp (vgcd champscorenum champscoreden))
                                  (setq champscorenum (quotient champscorenum
								temp))
                                  (setq champscoreden (quotient champscoreden
								temp))
                                  (setq champ p)))))
             (return champ))) 

(defun randomboolz* nil nil) 
;
;	sign*  --  return sign of numeric value
;	
;	will accept a bignum, but always returns a fixnum.
;
(defun sign* (n)
	(cond 	((lessp 0 n) 1)
		((equal 0 n) 0)
		(-1)))
;
;	onesignz  --  returns sign of something
;
;	onesignz is always used in fixnum comparisons and must return a fixnum.
;
(defun onesignz* (i) 
       (cond 	((= (col (left (baserowz* i))) onecolz*) 
			(sign*  (num (left (baserowz* i))))) 
		(t 0))) 

(defun anypivotz* (i) 
       (prog (p) 
             (setq p (left (baserowz* i)))
             (and (= (col (left p)) onecolz*) (setq p (left p)))
             (return (cond ((> (col p) dcolz*) p))))) 

(defun onesigniszeroz* (i) (not (= (col (left (baserowz* i))) onecolz*))) 

(defun negaterowz* (i) 
       (do ((p (left (baserowz* i)) (left p)))
	   ((= (col p) 0)) (xnum p (difference (num p))))) 

(defun crossingparamz* (piv i) 
       (prog (p n1 n2) 
             (setq p (left (baserowz* (row piv))))
             (or (= (col p) onecolz*) (return -1))
             (setq n1 (num p))
             (setq n2 (den p))
             (setq p (left (baserowz* i)))
             (return (difference (multiply n1 (den p) (num savepivotz*) (den piv)) 
                                 (multiply n2 (num p) (den savepivotz*) (num piv))))))

;;;============================================================================
(setq zmarkz* (ncons nil)) 

(defun markz* (p) (xownerz* p (cons zmarkz* (ownerz* p)))) 

(defun unmarkallz* nil 
       (do ((i (1+ drowz*) (1+ i))) ((> i lastrowz*)) (um2z* (baserowz* i)))
       (do ((j (1+ dcolz*) (1+ j))) ((> j lastcolz*)) (um2z* (basecolz* j)))) 

(defun um2z* (p) 
       (or (null (ownerz* p))
           (isenode (ownerz* p))
           (not (eq (car (ownerz* p)) zmarkz*))
           (xownerz* p (cdr (ownerz* p))))) 

(defun rowopz* (x1 x2 coefnum coefden) 
       (do ((p1 (left x1) (left p1)) (p2 x2) (q))
           ((not (> (col p1) dcolz*)))
           (setq p2 (rightz* (col p1) p2))
           (cond ((= (col (left p2)) (col p1))
                  (setq q (left p2))
                  (xnum q (add (multiply (num q) coefden (den p1))
			       (multiply (den q) coefnum (num p1))))
                  (xden q (multiply (den q) coefden (den p1)))
                  (fix0z* q)
                  (and (zerop (num q))
                       (progn (delfromcolz* q (col p1))
                              (makeavailz* q)
                              (xleft p2 (left q)))))
                 (t (setq q (nodez* (multiply (num p1) coefnum)
                                    (multiply (den p1) coefden)
                                    nil
                                    (left p2)
                                    (row p2)
                                    (col p1)
                                    nil
                                    0))
                    (fix0z* q)
                    (xleft p2 q)
                    (linkintocolz* q (col p1)))))) 

(defun inctableauelemz* (x y coefnum coefden) 
       (prog (p q r) 
             (setq p (rightz* (col y) x))
             (setq q (downz* (row x) y))
             (and (= (col (left p)) (col y))
                  (progn (setq r (left p))
                         (xnum r (add (multiply (num r) coefden)
				      (multiply (den r) coefnum)))
                         (xden r (multiply (den r) coefden))
                         (fix0z* r)
                         (or (zerop (num r)) (return nil))
                         (makeavailz* (up q))
                         (xup q (up (up q)))
                         (xleft p (left (left p)))
                         (return nil)))
             (setq r (nodez* coefnum coefden (up q) (left p) (row p) (col q) nil 0))
             (fix0z* r)
             (xup q r)
             (xleft p r))) 

;;;============================================================================
(defun linkintocolz* (p j) 
       (prog (q) (setq q (downz* (row p) (basecolz* j))) (xup p (up q)) (xup q p))) 

(defun delfromcolz* (p j) 
       (prog (q) 
             (setq q (downz* (row p) (basecolz* j)))
             (xup q (up (up q)))
             (or (eq (up q) q) (return nil))
             (swapcolz* (col q) lastcolz*)
             (makeavailz* q)
             (setq lastcolz* (1- lastcolz*)))) 

(defun pivotz* (p) (pivot2z* p) (exchangeownersz* p)) 

(defun exchangeownersz* (p) 
       (prog (y x downofy rightofx) 
             (setq y (basecolz* (col p))
                   x (baserowz* (row p))
                   downofy (downz* 0 p)
                   rightofx (rightz* 0 p))
             (xcol x (col y))
             (xrow y (row x))
             (xrow x 0)
             (xcol y 0)
             (xup x (up y))
             (xleft y (left x))
             (comment the old links are irrelevant and need not be made nil)
             (xup downofy x)
             (xleft rightofx y)
             (store (baserowz* (row p)) y)
             (store (basecolz* (col p)) x))) 

(defun fix0z* (p) 
       (prog (n) 
             (setq n (vgcd (num p) (den p)))
             (xnum p (quotient (num p) n))
             (xden p (quotient (den p) n)))) 

(defsmac s1z* ()
         (setq i0 (row p))
         (setq j0 (col p))
         (setq alphanum (den p))
         (setq alphaden (num p))
         (cond ((lessp alphaden 0)
                (setq alphanum (difference alphanum))
                (setq alphaden (difference alphaden))))
         (xnum p 1)
         (xden p 1)
         (setq p0 (baserowz* i0))
         (setq q0 (basecolz* j0))) 

;;;============================================================================
(defsmac s2z* ()
         (do nil
             (nil)
             (setq p0 (left p0))
             (setq j (col p0))
             (and (= j 0) (return nil))
             (store (ptrz* j) (basecolz* j))
             (xnum p0 (multiply alphanum (num p0)))
             (xden p0 (multiply alphaden (den p0)))
             (fix0z* p0))) 

(defsmac s5z* ()
         (do nil (nil) (or (> (col p1) j) (return nil)) (setq p p1) (setq p1 (left p)))) 

(defsmac s6z* ()
         (and (< (col p1) j)
              (progn (do nil
                         (nil)
                         (or (> (row (up (ptrz* j))) i) (return nil))
                         (store (ptrz* j) (up (ptrz* j))))
                     (setq x (nodez* 0 1 (up (ptrz* j)) p1 i j nil 0))
                     (xleft p x)
                     (xup (ptrz* j) x)
                     (setq p1 x)
                     (store (ptrz* j) x)))) 

(defsmac s7z* ()
         (xnum p1 (difference (multiply (num p1) (den q0) (den p0))
			      (multiply (den p1) (num q0) (num p0))))
         (xden p1 (multiply (den p1) (den q0) (den p0)))
         (fix0z* p1)) 

(comment the last test in s7 is made in the cond in s8) 

(defsmac s8z* ()
         (cond ((not (zerop (num p1)))
		(store (ptrz* j) p1) (setq p p1) (setq p1 (left p)))
               (t (do nil
                      (nil)
                      (or (> (row (up (ptrz* j))) i) (return nil))
                      (store (ptrz* j) (up (ptrz* j))))
                  (xup (ptrz* j) (up p1))
                  (xleft p (left p1))
                  (makeavailz* p1)
                  (setq p1 (left p))))) 

;;;============================================================================
(defun pivot2z* (p) 
       (prog (i0 j0 alphanum alphaden p0 q0 i j x p1) 
             (s1z*)
             (s2z*)
             (do nil
                 (nil)
                 (setq q0 (up q0))
                 (setq i (row q0))
                 (and (= i 0) (return nil))
                 (or (= i i0)
                     (progn (setq p (baserowz* i))
                            (setq p1 (left p))
                            (comment the last two setqs in s3 and all of s4-s8 are only
                                     performed if we are not at the pivotz* row)
                            (comment following cooresponds to s4)
                            (do nil
                                (nil)
                                (setq p0 (left p0))
                                (setq j (col p0))
                                (and (= j 0)
                                     (progn (xnum q0
						  (difference
						   (multiply alphanum (num q0))))
                                            (xden q0 (multiply alphaden (den q0)))
                                            (fix0z* q0)
                                            (return nil)))
                                (or (= j j0) (progn (s5z*) (s6z*) (s7z*) (s8z*))))))))) 

(defun swaprowz* (i1 i2) 
       (prog (XX)
             (and (= i1 i2) (return nil))
             (and (> i1 i2) 
                  (setq XX i2)
                  (setq i2 i1)
                  (setq i1 XX))
             (do ((p1 (left (baserowz* i1))) (p2 (left (baserowz* i2))) (q1) (q2) (n))
                 (nil)
                 (setq n (max (col p1) (col p2)))
                 (cond ((= n 0)
                        (store (baserowz* i1)
                               (prog2 nil
                                      (baserowz* i2)
                                      (store (baserowz* i2) (baserowz* i1))))
                        (xrow p1 i2)
                        (xrow p2 i1)
                        (return nil)))
                 (setq q2 (downz* i2 (basecolz* n)))
                 (setq q1 (downz* i1 q2))
                 (cond ((= (col p1) (col p2))
                        (cond ((eq q1 p2) (xup p2 (up p1)) (xup q2 p1) (xup p1 p2))
                              (t (xup p1 (prog2 nil (up p2) (xup p2 (up p1))))
                                 (xup q1 (prog2 nil (up q2) (xup q2 (up q1))))))
                        (xrow p1 i2)
                        (xrow p2 i1)
                        (setq p1 (left p1))
                        (setq p2 (left p2)))
                       ((> (col p1) (col p2))
                        (cond ((eq q2 p1))
                              (t (xup q1 (up p1)) (xup p1 (up q2)) (xup q2 p1)))
                        (xrow p1 i2)
                        (setq p1 (left p1)))
                       (t (cond ((eq q1 p2))
                                (t (xup q2 (up p2)) (xup p2 (up q1)) (xup q1 p2)))
                          (xrow p2 i1)
                          (setq p2 (left p2))))))) 
;;;============================================================================
(defun swapcolz* (j1 j2) 
       (prog nil 
             (and (= j1 j2) (return nil))
             (and (> j1 j2) (setq j1 (prog2 nil j2 (setq j2 j1))))
             (do ((p1 (up (basecolz* j1))) (p2 (up (basecolz* j2))) (q1) (q2) (n))
                 (nil)
                 (setq n (max (row p1) (row p2)))
                 (cond ((= n 0)
                        (store (basecolz* j1)
                               (prog2 nil
                                      (basecolz* j2)
                                      (store (basecolz* j2) (basecolz* j1))))
                        (xcol p1 j2)
                        (xcol p2 j1)
                        (return nil)))
                 (setq q2 (rightz* j2 (baserowz* n)))
                 (setq q1 (rightz* j1 q2))
                 (cond ((= (row p1) (row p2))
                        (cond ((eq q1 p2) (xleft p2 (left p1)) (xleft q2 p1) (xleft p1 p2))
                              (t (xleft p1 (prog2 nil (left p2) (xleft p2 (left p1))))
                                 (xleft q1 (prog2 nil (left q2) (xleft q2 (left q1))))))
                        (xcol p1 j2)
                        (xcol p2 j1)
                        (setq p1 (up p1))
                        (setq p2 (up p2)))
                       ((> (row p1) (row p2))
                        (cond ((eq q2 p1))
                              (t (xleft q1 (left p1)) (xleft p1 (left q2)) (xleft q2 p1)))
                        (xcol p1 j2)
                        (setq p1 (up p1)))
                       (t (cond ((eq q1 p2))
                                (t (xleft q2 (left p2))
                                   (xleft p2 (left q1))
                                   (xleft q1 p2)))
                          (xcol p2 j1)
                          (setq p2 (up p2))))))) 

(defun newcolz* nil 
       (prog (y) 
             (or (< lastcolz* (1- onecolz*)) (morecolsz*))
             (setq lastcolz* (1+ lastcolz*))
             (setq y (nodez* nil nil nil nil 0 lastcolz* nil 0))
             (xup y y)
             (store (basecolz* lastcolz*) y))) 

(defun newrowz* (terms) 
       (prog (x) 
             (or (< lastrowz* maxrowz*) (morerowsz*))
             (setq lastrowz* (1+ lastrowz*))
             (setq x (nodez* nil nil nil nil lastrowz* 0 nil 0))
             (xownerz* x terms)
             (xleft x x)
             (store (baserowz* lastrowz*) x))) 

(defun morerowsz* nil (break (cant extend new z context))) 

(defun morecolsz* nil (break (cant extend new z context))) 

;;;============================================================================
(defun nodez* (num den up left row col prehistory pushcount) 
       (prog (temp) 
             (setq temp (getnodez*))
             (xnum temp num)
             (xden temp den)
             (xup temp up)
             (xleft temp left)
             (xrow temp row)
             (xcol temp col)
             (xprehistory temp prehistory)
             (xpushcount temp pushcount)
             (return temp))) 

(defun getnodez* nil 
       (prog2 (or availz* (extendavailz*)) availz* (setq availz* (den availz*)))) 

(defun extendavailz* nil 
       (setq availz* (alloc-node))
       (do ((p availz* (den p)) (i 1 (1+ i))) ((> i 50)) (xden p (alloc-node)))) 

;(defun extendavailz* nil
;       (setq availz* (alloc-node))
;       (extendavailz2* 50 availz*))

;(defun extendavailz2* (n x)
;       (or (= n 0) (xden x (extendavailz2* (1- n) (alloc-node)))))

(defun makeavailz* (n) (xden n availz*) (setq availz* n)) 

(defun initz nil 
       (oldstylearray baserowz* t 310) 
       (oldstylearray basecolz* t 310) 
       (oldstylearray ptrz* t 310) 
       (setq availz* nil)
       (setq lastrowz* 0)
       (setq lastcolz* 1)
       (setq maxrowz* 307)
       (setq maxcolz* 307)
       (setq drowz* 0)
       (setq dcolz* 0)
       (setq onecolz* maxcolz*)
       (store (basecolz* onecolz*) (nodez* 1 nil nil nil 0 onecolz* nil 0))
       (xup (basecolz* onecolz*) (basecolz* onecolz*))
       (setq floorz* (nodez* 0 nil nil nil 0 1 nil 0))
       (xup floorz* floorz*)
       (store (basecolz* 1) floorz*)
       (setq historyz* (basecolz* onecolz*))) 
