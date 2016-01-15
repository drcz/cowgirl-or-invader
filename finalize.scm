;;; basic new blockmap extraction from the closed process tree:

(define (rm-trans-label conf)
  (if (eq? (car conf) 'TRANSIT)
      (cdr conf)
      conf))

(define (tree->blockmap tree)
  (cons (cons (rm-trans-label (get-label-of tree))
	      (get-content-of tree))
	(append-map tree->blockmap (get-children-of tree))))

;;; change (pp . store) labels into symbols: 
(define (massage blockmap)
  (let* ((old-keys (map car blockmap))
	 (new-keys (map (lambda (k) ; blee, gensym is impure!
			  (gensym 
			   (cond-expand
			    (gambit (car k))
			    (guile-2 (symbol->string (car k))))))
			old-keys))
	 (mapping (map cons old-keys new-keys)))
    (let loop ((bm blockmap))
      (if (null? bm)
	  '()
	  (let* ((cur-node (car bm))
		 (old-label (car cur-node))
		 (old-block (cdr cur-node))
		 (new-label (lookup old-label mapping))
		 (new-block (massage-labels-in old-block mapping)))
	    (cons (cons new-label new-block)
		  (loop (cdr bm))))))))

(define (massage-labels-in block mapping)
  (match block
    [(('let v e) . block)
     `((let ,v ,e) . ,(massage-labels-in block mapping))]
    [(('return e)) block]
    [(('goto l)) `((goto ,(lookup l mapping)))]
    [(('if e l1 l2))
     `((if ,e ,(lookup l1 mapping) ,(lookup l2 mapping)))]))


;;; convert CVARs and TVARS into symbols
(define (mvars->symbols-in-program blockmap)
  (map (lambda (entry)
	 (cons (car entry)
	       (mvars->symbols-in-block (cdr entry))))
       blockmap))

(define (mvars->symbols-in-block block)
  (match block
    [(('return e))
     `((return ,(mvars->symbols-in-expr e))) ]
    [(('let mv e) . block)
     `((let ,(mvar->symbol mv) ,(mvars->symbols-in-expr e))
       . ,(mvars->symbols-in-block block))]
    [(('if e l1 l2))
     `((if ,(mvars->symbols-in-expr e) ,l1 ,l2))]
    [(('goto l))
     `((goto ,l))]))

(define (mvars->symbols-in-expr e)
  (match e
    [(? ground-sex? g) g]
    [('CVAR n) (mvar->symbol e)]
    [('TVAR n) (mvar->symbol e)]
    [(op e) `(,op ,(mvars->symbols-in-expr e))]
    [(op e1 e2) `(,op ,(mvars->symbols-in-expr e1) 
		      ,(mvars->symbols-in-expr e2))]))

(define (mvar->symbol mvar)
  (string->symbol (string-append (if (eq? (car mvar) 'CVAR) "x" "t") (number->string (cadr mvar)))))

;;; compressing trivial transitions...
(define (compress-transitions blockmap init-pp)
  (let ((jumpcounts (build-jumpinfo blockmap init-pp)))
    (let loop ((pend `(,(caar blockmap)))
	       (res '()))
      (if (null? pend)
	  (reverse res)
	  (let* ((cur-label (car pend))
		 (pend (cdr pend)))
	    (if (lookup cur-label res)
		(loop pend res)
		(let* ((cur-block (lookup cur-label blockmap))
		       (new-block (let compress ((block cur-block))
				    (match block
				      [(('let v e) . block)
				       `((let ,v ,e) . ,(compress block))]
				      [(('return e))
				       `((return ,e))]
				      [(('if e l1 l2))
				       `((if ,e ,l1 ,l2))]
				      [(('goto l))
				       (if (> 1 (lookup l jumpcounts))
					   `((goto ,l))
					   (compress (lookup l blockmap)))])))
		       (children (get-jumps-from new-block))
		       (res `((,cur-label . ,new-block) . ,res))
		       (pend (append children pend)))
		  (loop pend res))))))))

;; for every pp, count number of jumps to it (from gotos and ifs):
(define (count-occurences x xs)
  (cond ((null? xs) 0)
	((equal? x (car xs)) (+ 1 (count-occurences x (cdr xs))))
	(else (count-occurences x (cdr xs)))))

(define (build-jumpinfo blockmap init-pp)
  (let* ((all-pps (map car blockmap))
	 (all-blocks (map cdr blockmap))
	 (all-jumps (append-map get-jumps-from all-blocks))
	 (pps-jumpcounts (map (lambda (pp)
				(count-occurences pp all-jumps))
			      all-pps))
	 (jump-info (map cons all-pps pps-jumpcounts)))
    ;;; now the init-pp is called once more,
    ;;; when the program starts...
    (update init-pp
	    (+ 1 (lookup init-pp jump-info))
	    jump-info)))


;;; all together...

(define (finalize tree)
  (mvars->symbols-in-program 
   (massage 
    (compress-transitions   
      (tree->blockmap tree)
      (get-label-of tree)))))
