"@(#)fixes.l	1.1"
;
;	fixes.l  --  overrides loaded last
;
;	THIS FILE MUST NOT BE COMPILED
;
;	It is a workaround for a bug in the SUN version of Franz Lisp, which
;	will not compile the functions below correctly.  Bad code is
;	generated resulting in clobbered memory if these functions are
;	compiled on a SUN, although on a VAX they work compiled.
;	They work fine in interpreted mode.
;
(declare (load 'need.o)
	 (load 'defmac.o)
	 (load 'hunkshell.o)
	 (load 'enode.o)
	 (load 'debug.o))

(needs-macros)

(hunkshell node num den up left row col prehistory pushcount)

(defsmac ownerz* (p) (num p)) 

(defsmac xownerz* (p new) (xnum p new)) 

(defmac rightz* (j p) (do ((q p (left q))) ((not (> (col (left q)) j)) q))) 

(defmac downz* (i p) (do ((q p (up q))) ((not (> (row (up q)) i)) q))) 

;;;============================================================================

;;;  END MACROS, begin CODE
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

