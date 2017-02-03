;
;	Some test cases.  These require the rules given here.
;
;
;	The declarations
;
(setq testdecls '(
(x~1 (variable (subrange 1 100)))
(y~2 (variable (subrange 1 100)))
(z~3 (variable (subrange 1 100)))
(b~4 (variable (boolean)))
(a~5 (variable (array (subrange 1 100) (subrange 0 300))))
(u~1 (variable (universal)))
(f (variable (integer)))
(p (function (boolean)))
(u (function (universal)))
(allzero (rulefunction (boolean)))
(dummyfff (rulefunction (boolean)))
(dummyggg (rulefunction (boolean)))
(rec~6
(variable (record buffer~1 
	(buffer~1$bufin (subrange 0 19)) 
	(buffer~1$bufout (subrange 0 19)) 
	(buffer~1$bufcount (subrange 0 20)) 
	(buffer~1$buf (array (subrange 0 19) (subrange 0 255))))))
))
;
;	The rules
;
(setq testrules
      (quote
       (
		(|prove-lemma| |simple-dummy-rule|	; dummy rule to test if rule engine is working at all
                       (rewrite)
                       (implies (dummyfff A)
									(dummyggg B)))
	    (|prove-lemma| |unchanged-allzero-rule|
                       (rewrite)
                       (implies (and (numberp I)
                                     (numberp J)
                                     (numberp X)
                                     (arrayp! A)
                                     (allzero A I J)
                                     (equal B (storea! A X V))
                                     (or (lessp X I) (lessp J X)))
                                (allzero B I J)))
        (|prove-lemma| |allzero-extend-rule|
                       (rewrite)
                       (implies (and (numberp I)
                                     (numberp J)
                                     (arrayp! A)
                                     (allzero A I J)
                                     (equal (selecta! A (plus 1 J)) 0))
                                (allzero A I (plus 1 J))))
        (|prove-lemma| |allzero-first-rule|
                       (rewrite)
                       (implies (and (numberp I)
                                     (arrayp! A)
                                     (equal (selecta! A I) 0))
                                (allzero A I I)))
        (|prove-lemma| |allzero-select-rule|
                       (rewrite)
                       (implies (and (numberp I)
                                     (numberp J)
                                     (numberp X)
                                     (arrayp! A)
                                     (not (lessp J X))
                                     (not (lessp X I))
                                     (allzero A I J))
                                (equal (selecta! A X) 0))))))
;
;	The test cases
;
(setq testlist nil)
(testcase 'f1  nil '(equal! (x~1v01) (consti! 1)))	; not true
(testcase 'f2  nil '(equal! (x~1v01) (y~2v02)))		; not true
(testcase 'f3  t '(implies! (and! 	(equal! (x~1v01) (y~2v01))
				(equal! (y~2v01) (z~3v01)))
			(equal! (z~3v01) (x~1v01))))
(testcase 'f4 t '(implies! (and! 	(gei! (x~1v01) (y~2v01))
				(gei! (y~2v01) (z~3v01)))
			(gei! (x~1v01) (z~3v01))))
(testcase 'f5 nil '(implies! (and! 	(gei! (x~1v01) (y~2v01))
				(gei! (y~2v01) (z~3v01)))
			(gei! (z~3v01) (x~1v01))))
;
;	Predicate tests
;
(testcase 'p01 t '(integerp! (x~1v01)))
(testcase 'p02 nil '(booleanp! (x~1v01)))
(testcase 'p03 nil '(arrayp! (x~1v01)))
(testcase 'p11 nil '(integerp! (b~4v01)))
(testcase 'p12 t '(booleanp! (b~4v01)))
(testcase 'p13 nil '(arrayp! (b~4v01)))
(testcase 'p21 nil '(integerp! (a~5v01)))
(testcase 'p22 nil '(booleanp! (a~5v01)))
(testcase 'p23 t '(arrayp! (a~5v01)))
(testcase 'p31 t '(numberp! (consti! 50)))
(testcase 'p32 nil '(numberp! (consti! -50)))
;
;	Array tests
;
(testcase 'a01 t '(equal! (selecta! (storea! (a~5v01) (x~1v01) (y~2v01))
		   	(x~1v01)) (y~2v01)))
(testcase 'a02 nil '(equal! (selecta! (storea! (a~5v01) (x~1v01) (y~2v01))
		   	(b~4v01)) (y~2v01)))
(testcase 'a03 t '(implies! (and! 
			 (equal! (u (consti! 2)) (a~5v01))
			(and!
			 (equal! (consti! 2) (z~3v01))
			(equal! 
			   (selecta! (storea! (u (z~3v01)) (x~1v01) (y~2v01)) 
				(x~1v01))
			   (z~3v01))))
		     (equal! (y~2v01) (z~3v01))))
;
;	record tests
;
(testcase 'r01 t '(equal! (x~1v01) 
			(selectr! (storer! (rec~6v01) buffer~1 bufcount
						(x~1v01)) buffer~1 bufcount)))
(testcase 'r02 t '(not! (arrayp! (rec~6v01))))
(testcase 'r03 t '(integerp! (selectr! (rec~6v01) buffer~1 bufcount)))
;
;	Arithmetic tests
;
(testcase 'z01 t '(equal! (addi! 2 2) 4))
(testcase 'z02 t '(equal! (addn! 2 2) 4))
(testcase 'z03 t '(equal! (addi! -2 -2) -4))
(testcase 'z04 nil '(equal! (addn! -2 -2) -4))
(testcase 'z05 t '(equal! (addi! (x~1v01) (y~2v02)) (addi! (y~2v02) (x~1v01))))
(testcase 'z06 nil '(equal! (addi! (u~1v01) (y~2v02)) (addi! (y~2v02) (u~1v01))))
(comment 'z07 t '(implies!
			(integerp! (u~1v01))
			(equal! (addi! (u~1v01) (y~2v02)) 
				(addi! (y~2v02) (u~1v01)))))
(testcase 'z08 t '(implies!
			(equal! (u~1v01) (x~1v01))
			(equal! (addi! (u~1v01) (y~2v02)) 
				(addi! (y~2v02) (u~1v01)))))
;
;	multiply bounds tests
;
(testcase 'm01 t '(implies!
			(equal! (x~1v01) 2)
			(equal! (muli! (x~1v01) (x~1v01)) 4)))
;
;
;	rule tests
;
(testcase 'x01 t '(implies! (dummyfff x~1v01) (dummyggg x~1v01)))	; simplest possible rule test

(testcase 'x02 nil '(implies! (dummyggg x~1v01) (dummyfff x~1v01)))	; simplest possible rule test

(testcase 'u01 t '(implies! (allzero (a~5v01) (consti! 1) (consti! 50))
	             (allzero (storea! (a~5v01) (consti! 51) (consti! 10))
			      (consti! 1) (consti! 50))))

(testcase 'u02 nil '(implies! (allzero (a~5v01) (consti! 1) (consti! 50))
	             (allzero (storea! (a~5v01) (consti! 50) (consti! 10))
			      (consti! 1) (consti! 50))))
