;;; TODO: krótki opis, też


;;; when operating on configurations, we assume the metastores have
;;; the variables in the same order -- whenever in doubt use this one:
(define (reconcile-variables-order desired current)
  (let rebuild ((vars (map car desired)))
    (if (null? vars)
	'()
	(cons (assoc (car vars) current)
	      (rebuild (cdr vars))))))


(define (configuration-subset? config-1 config-2) ;; 1 is subset of 2
  (and (equal? (get-pp-of-config config-1)
	       (get-pp-of-config config-2))
       (is-instance-of? (get-meta-store-of-config config-1)
			(get-meta-store-of-config config-2))))

(define (is-instance-of? meta-store-1 meta-store-2) ;; 1 can be transformed to 2 
  (let check ((vars (map car meta-store-1))
	      (binding '()))
    (if (null? vars)
	binding #;#t
	(let* ((var (car vars))
	       (v1 (lookup var meta-store-1))
	       (v2 (lookup var meta-store-2))
	       (binding-or-false
		(let check-expr ((v1 v1)
				 (v2 v2)
				 (binding binding))
		  (match `(,v1 ,v2)
		    [(expr ('CVAR n))
		     (let ((val (lookup n binding)))
		       (if val
			   (and (equal? expr val) binding)
			   (update n expr binding)))]
		    [(('KONST k1) ('KONST k2))
		     (if (equal? k1 k2) binding #f)]
		    [((op a1) (op a2)) (check-expr a1 a2 binding)]
		    [((op a1 b1) (op a2 b2))
		     (let ((binding (check-expr a1 a2 binding)))
		       (and binding (check-expr b1 b2 binding)))]
		    [otherwise
		     #f]))))
	  (and binding-or-false
	       (check (cdr vars) binding-or-false))))))

#;(is-instance-of?
 '[(m . (CVAR 0)) (n . (- (CVAR 1) (KONST 1))) (r . (* (CVAR 0) (CVAR 0)))]
 '[(m . (CVAR 0)) (n . (CVAR 1)) (r . (CVAR 0))])


;;; keeps "fresh" c-var fresh wrt "expr":
(define (update-fresh fresh expr)
  (let* ((cvars-in-expr (all-cvars-in expr))
	 (cvars-indexes (cons -1 (map cadr cvars-in-expr)))
	 (max-index (apply max cvars-indexes))
	 (fresh-old-index (cadr fresh)))
    (if (>= max-index fresh-old-index)
	`(CVAR ,(+ 1 max-index))
	fresh)))
	 

;;; the following assumes:
;;; -- both configs have the same variables order (cf reconcile-variables-order),
;;; -- config-2 is config-1's ancestor.
(define (whistle? config-1 config-2)
  (and (equal? (get-pp-of-config config-1)
	       (get-pp-of-config config-2))
       (let* ((meta-store-1 (get-meta-store-of-config config-1))
	      (meta-store-2 ;; secure variant?
	       (reconcile-variables-order meta-store-1
					  (get-meta-store-of-config config-2)))
	     #;(meta-store-2 (get-meta-store-of-config config-2)))
	 (let check ((vars (map car meta-store-1)))
	   (if (null? vars)
	     #t
	(let* ((var (car vars))
	       (v1 (lookup var meta-store-1))
	       (v2 (lookup var meta-store-2))
	       (v1-is-close-to-v2
		(let close? ((v1 v1)
			     (v2 v2))
		  (match `(,v1 ,v2)
		    [(('KONST k1) ('KONST k2))
		     (or (equal? k1 k2)
			 (and (number? k1)
			      (number? k2)
			      (< (abs k2) (abs k1))))]
		    [(('CVAR n) ('CVAR m)) #t]
		    [(('CVAR n) e) #f] ;; sure?
		    [(e ('CVAR n)) #t]
		    [((op a1) (op a2))
		     (or (close? a1 a2)
			 (close? a1 `(,op ,a2)))]
		    [((op a1 b1) (op a2 b2))
		     (or (and (close? a1 a2)
			      (close? b1 b2))
			 (close? a1 `(,op ,a2 ,b2))
			 (close? b1 `(,op ,a2 ,b2)))]
		    [((op a) e)
		     (close? a e)]
		    [((op a b) e)
		     (or (close? a e)
			 (close? b e))]
		    [otherwise #f]))))
	  (if v1-is-close-to-v2
	      (check (cdr vars))
	      #f)))))))

#;(whistle? '(tmp . [(m . (CVAR 0)) (n . (- (CVAR 1) (KONST 1))) (r . (* (CVAR 0) (CVAR 0)))])
	  '(tmp . [(m . (CVAR 0)) (n . (CVAR 1)) (r . (CVAR 0))]))


;; actualy this is reverse of substitution in "introduction to supercompilation",
;; applied to c1 "produces" c2 [eg c1 is child, c2 is ancestor, making jump back]
;;; TODO now can be simplified with the output of "is-instance-of?"...
(define (mk-substitution-from config-1 config-2)
  (let* ((c1-cvars (gather-all-cvars config-1))
	 (c2-cvars (gather-all-cvars config-2))
	 (tmp-vars (map (lambda (i) `(TVAR ,i))
			(iota (length c2-cvars))))
	 (c2->tmp-mapping (map cons c2-cvars tmp-vars))
	 (c1->tmp-substitutions
	  (let S ((c1 config-1)
		  (c2 config-2))
	    (match `(,c1 ,c2)
	      [(expr ('CVAR n))
	       (let ((tmp-var (lookup `(CVAR ,n) c2->tmp-mapping)))
		 `((let ,tmp-var ,(metasex->sex expr))))]
	      [((h1 . t1) (h2 . t2))
	       (append (S h1 h2) (S t1 t2))]
	      [otherwise '()])))
	 (tmp->c2-substitution
	  (map (lambda (c2-var)
		 `(let ,c2-var ,(lookup c2-var c2->tmp-mapping)))
	       c2-cvars)))
    (append c1->tmp-substitutions
	    tmp->c2-substitution)))


;;; find the most specific generalization of two meta-stores:
(define (generalize meta-store-1 meta-store-2)
  (let build ((vars (map car meta-store-1))
	      (fresh-cvar '(CVAR 0)))
    (if (null? vars)
	'()
	(let* ((var (car vars))
	       (c1 (lookup var meta-store-1))
	       (c2 (lookup var meta-store-2))
	       (gen (let G ((c1 c1)
			    (c2 c2)
			    (fresh-cvar fresh-cvar)) ; ?!
		      (match `(,c1 ,c2)
			[(('KONST n) ('KONST n)) `(KONST ,n)]
			[((op a1 b1) (op a2 b2))
			 (let* ((gen-a (G a1 a2 fresh-cvar))
				(fresh-cvar (update-fresh fresh-cvar gen-a))
				(gen-b (G b1 b2 fresh-cvar)))
			   `(,op ,gen-a ,gen-b))]
			[otherwise fresh-cvar])))
	       (fresh-cvar (update-fresh fresh-cvar gen)))
	  `((,var . ,gen)
	    . ,(build (cdr vars) fresh-cvar))))))

