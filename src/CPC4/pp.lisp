"@(#)pp.l	2.1"

;;; YAPP - Yet Another Pretty Printer

;;; The following algorithm for pretty printing requires time linear in the
;;; length of the input and space linear in the linewidth of the output medium.
;;; It does not wait for the entire expression to be input, but begins printing
;;; as soon as it has received (roughly) a linefull of output.   A complete
;;; description of the algorithm is available in hardcopy from me.   The 
;;; following assumes you have read the hardcopy which explains the distinctions
;;; between consistent and inconsistent breaking, etc.    A simple and very
;;; hastily-written unparser for formulas using the pretty printer is 
;;; in pform.l

;;; To use, first initialize by calling *ppinit(k) where k is the linewidth of
;;; your output medium.   You then make successive calls to the following 
;;; functions:

;;; *ppstring(x, l) prints a string of length l on the output device.  (The
;;; value of l must equal the length of x.)

;;; *ppbegin(offset) Defines the start of a logical block of output tokens.  The
;;; algorithm will break as few such blocks as possible.  The parameter offset 
;;; is the offset for this block.   If sub-blocks of this block have to be 
;;; broken between lines, each will be indented k spaces from the indentation of
;;; the start of the block.

;;; *ppend() Defines the end of a logical block.   Occurrences of *ppbegin() and
;;; *ppend must be balanced.   

;;; *ppblank(type offset space) defines a point where the pretty printer may
;;; insert a line break if necessary; if it isn't necessary, a blank of length
;;; zero or more will be inserted.   If the variable type is NIL, then the 
;;; linebreaks are inconsistent --- the block will be broken in as few places
;;; as possible.   If T, the breaks are consistent --- if one breaks, all break.
;;; E. g.       inconsistent            consistent
;;;                f(a, b,                 f(a,
;;;                  c)                      b,
;;;                                          c)
;;; assuming a line width of 7!   Offset is the space to be indented from the
;;; start of the current block if breaking is necessary.   If breaking is not
;;; necessary, zero or more spaces (the number given by the variable space) are
;;; inserted.

;;; *ppcr(offset) inserts a carriage return and indents relative offset.

;;; *ppeof() flushes all remaining input to the output device and inserts
;;; a final carriage return.

;;;============================================================================

;;;(declare (load 'need.o) (load 'princ.o)
(declarespecial *margin *left *right *leftotal *rightotal *space *marginerrors
                  *top *bottom *pstack *sstackempty *end *arraysize)
;;;(needs-macros)

(setq *margin 10 *left 1 *right 1 *leftotal 1 *rightotal 1 *top 1 *bottom 1)
(setq *pstack nil *sstackempty t *space *margin *end '(end) *marginerrors 0)

(defun   *newline (k) (terpri) (*indent k))

(defun   *indent (k) (do ((i 0 (1+  i))) ((= i k)) (princ '" ")))

(defun *pprint (x l)
  (cond ((atomp x) 
	 (and (> l *space) (setq *marginerrors (1+ *marginerrors)))
	 (princ x) 
	 (setq *space (- *space l)))
	((eq (car x) 'begin)
	 (setq *pstack (cond ((> l *space)
			      (cons (- *space (cdr x)) *pstack))
			     (t (cons -1 *pstack)))))
	((eq x *end) (or *pstack (break "Prettyprinter error: popop")) (setq *pstack (cdr *pstack)))
	((eq (car x) nil)
	 (cond ((> l *space)
		(setq *space (- (car *pstack) (cadr x)))
		(*newline (- *margin *space)))
	       (t (*indent (caddr x))
		  (setq *space (- *space (caddr x))))))
	((eq (car x) t)
	 (cond ((> (car *pstack) -1)
		(setq *space (- (car *pstack) (cadr x)))
		(*newline (- *margin *space)))
	       (t (*indent (caddr x))
		  (setq *space (- *space (caddr x))))))
	(t (break "Prettyprinter error: pprint")))
  t)

(defun *push (x)
       (cond (*sstackempty (setq *sstackempty nil *top 1 *bottom 1))
             (t (setq *top (cond ((= *top *arraysize) 1) (t (1+ *top))))
                (and (= *top *bottom) (break "Prettyprinter error: stack overflow"))))
       (store (*sstack *top) x))

(defun *pop()
       (prog2 (and *sstackempty (break "Prettyprinter error: *pop"))
              (*sstack *top)
              (cond ((= *top *bottom) (setq *sstackempty t *top 1 *bottom 1))
                    (t (setq *top (cond ((= *top 1) *arraysize) (t (1- *top))))))))

(defun *top () (cond (*sstackempty (break *top)) (t (*sstack *top))))

(defun *popbottom ()
       (prog2 (and *sstackempty (break *popbottom))
              (*sstack *bottom)
              (cond ((= *top *bottom) (setq *sstackempty t))
                    (t (setq *bottom (cond ((= *bottom *arraysize) 1) (t (1+ *bottom))))))))
       
(defun *advanceright ()
       (setq *right (cond ((= *right *arraysize) 1) (t (1+ *right))))
       (and (= *left *right) (break "Prettyprinter error: stream overflow")))
;;;============================================================================
(defun *ppinit (k)
       (setq *margin k)
       (setq *arraysize (* 5 *margin))
       (oldstylearray *sstack 'fixnum (1+ *arraysize)) ; CL
       (oldstylearray *size 'fixnum (1+ *arraysize)) ; CL
       (oldstylearray *stream t (1+ *arraysize)) ; CL
       (setq *sstackempty t)
       (setq *leftotal 1 *rightotal 1 *bottom 1 *top 1 *left 1 *right 1)
       (setq *space *margin *pstack nil *marginerrors 0))

(defun *ppbegin (offset)
       (cond (*sstackempty (setq *right 1 *left 1 *leftotal 1 *rightotal 1))
             (t (*advanceright)))
       (store (*stream *right) (cons 'begin offset))
       (store (*size *right) (- *rightotal))
       (*push *right)
       t)

(defun *ppend () 
       (cond (*sstackempty (*pprint *end 0))
             (t (*advanceright)
                (store (*stream *right) *end)
                (store (*size *right) -1)
                (*push *right)))
       t)

(defun *ppblank (x offset space)
       (cond (*sstackempty (setq *right 1 *left 1 *leftotal 1 *rightotal 1))
             (t (*advanceright)))
       (*checkstack 0)
       (*push *right)
       (store (*stream *right) (list x offset space))
       (store (*size *right) (- *rightotal))
       (setq *rightotal (+ space *rightotal))
       t)

(defun *ppstring (x l)
       (cond (*sstackempty (*pprint x l))
             (t (*advanceright)
                (store (*stream *right) x)
                (store (*size *right) l)
                (setq *rightotal (+ *rightotal l))
                (*checkstream)))
       t)

(defun *ppcr (offset)
       (*ppblank t offset *margin))

(defun *ppeof ()
       (cond (*sstackempty)
             (t (*checkstack 0)
                (*advanceleft (*stream *left) (*size *left))))
       (terpri)
       (setq *leftotal 1 *rightotal 1 *bottom 1 *top 1 *left 1 *right 1)
       (setq *sstackempty t *space *margin *pstack nil))

;;;============================================================================
(defun *advanceleft (x l)
       (cond ((> l -1)
              (*pprint x l)
              (cond ((atomp x) (setq *leftotal (+ *leftotal l)))
                    ((eq (car x) 'begin))
                    ((eq x *end))
                    (t (setq *leftotal (+ (caddr x) *leftotal))))
              (and (> *leftotal *rightotal) (break "Prettyprinter error: advanceleft"))
              (cond ((= *left *right))
                    (t (setq *left (cond ((= *left *arraysize) 1) (t (1+ *left))))
                       (*advanceleft (*stream *left) (*size *left)))))))

(defun *checkstack (k)
       (or *sstackempty
           (prog (x)
                 (setq x (*top))
                 (cond ((eq (car (*stream x)) 'begin)
                        (cond ((> k 0) 
                               (store (*size (*pop)) (+ (*size x) *rightotal))
                               (*checkstack (1- k)))))
                       ((eq (*stream x) *end) 
                        (store (*size (*pop)) 1)
                        (*checkstack (1+ k)))
                       (t (store (*size (*pop)) (+ (*size x) *rightotal))
                          (and (> k 0) (*checkstack k)))))))

(defun *checkstream ()
       (cond ((> (- *rightotal *leftotal) *space)
              (and (not *sstackempty) (= *left (*sstack *bottom))
                   (store (*size (*popbottom)) 900))
              (*advanceleft (*stream *left) (*size *left))
              (or (= *left *right) (*checkstream)))))

;;; Prettyprinter state dump for debug
(defun *ppp()
       (princ-terpri '"left     right   bottom  top     stack?  space   leftot  rightot")
       (princ-tab *left)
       (princ-tab *right)
       (princ-tab *bottom)
       (princ-tab *top)
       (princ-tab (cond (*sstackempty '"No") (t '"Yes")))
       (princ-tab *space)
       (princ-tab *leftotal)
       (princ-terpri *rightotal)
       (terpri)
       (princ-terpri '" stream  size    stack")
       (do ((i 1 (1+ i))) ( (= i (1+ *arraysize)))
           (princ-tab i) (princ-tab (*stream i)) (princ-tab (*size i))
           (princ-terpri (*sstack i)))
       (terpri)
       (princ *pstack))
