;;; this is IT:

;[define (dbg-print msg) (write msg) (newline)]

(define (drive-block block meta-store)
  (match block
    [(('goto pp))
     `((goto ,(mk-config pp meta-store)))]

    [(('return e))
     `((return ,(metasex->sex (abstract-eval e meta-store))))]

    [(('if e pp1 pp2))
     (match (abstract-eval e meta-store)
       [('KONST ())
	`((goto ,(mk-config pp2 meta-store)))]
       [('KONST T)
	`((goto ,(mk-config pp1 meta-store)))]
       [me
	`((if ,(metasex->sex me)
	      ,(mk-config pp1 meta-store)
	      ,(mk-config pp2 meta-store)))])]

    [(('let v e) . block)
     (drive-block block
		  (update v
			  (abstract-eval e meta-store)
			  meta-store))]))


(define (get-jumps-from block)
  (match block
    [(('goto pp)) `(,pp)]
    [(('if _ pp1 pp2)) `(,pp1 ,pp2)]
    [(('return _)) '()]
    [(('let . _) . block) (get-jumps-from block)]))


(define (mk-open-node config) (mk-node config 'OPEN '()))

(define (open-node? node) (eq? (get-content-of node) 'OPEN))


(define (unfold-tree leaf tree block-map)
;  (dbg-print `(UNFOLD ,(get-label-of leaf)))
  (let* ((config (get-label-of leaf))
	 (pp (get-pp-of-config config))
	 (meta-store (get-meta-store-of-config config))
	 (block (lookup pp block-map))
	 (new-block (drive-block block meta-store))
	 (children (map mk-open-node
			(get-jumps-from new-block)))
	 (new-node (mk-node config
			    new-block
			    children)))
    (substitute-in-tree config new-node tree)))


(define (fold-tree leaf ancestor tree)
;  (dbg-print `(FOLD ,(get-label-of leaf) to ,ancestor)) ;; !!
  (let* ((config (get-label-of leaf))
	 #;(pp (get-pp-of-config config))
	 (meta-store (get-meta-store-of-config config))
	 (anc-meta-store (get-meta-store-of-config ancestor))
	 (substitutions (mk-substitution-from meta-store
					      anc-meta-store))
	 (new-block (append substitutions
			    `((goto ,ancestor))))
	 (new-node (mk-node config
			    new-block
			    '())))
    (substitute-in-tree config new-node tree)))


(define (trivial-metastore? meta-store)
  (match meta-store
    [()
     #t]
    [((v . ('CVAR n)) . meta-store)
     (trivial-metastore? meta-store)]
    [otherwise #f]))
;(trivial-metastore? '[(x . (CVAR 0)) (y . (CVAR 1))])
;(trivial-metastore? '[(x . (CVAR 0)) (y . (cons (CVAR 1) (CVAR 0)))])

;;; todo : more DRY?
(define (generalize-tree leaf ancestor tree)
  (let* ((config (get-label-of leaf))
	 (pp (get-pp-of-config config))
	 (meta-store (get-meta-store-of-config config))
	 (anc-meta-store (get-meta-store-of-config ancestor))
	 (gen-meta-store (generalize meta-store
				     anc-meta-store))
	 (gen-config (mk-config pp gen-meta-store)))
#;  (dbg-print `(GEN ,(get-label-of leaf) with ,ancestor to ,gen-config eq= ,(or (equal? meta-store anc-meta-store) ;; !!
	    (trivial-metastore? gen-meta-store))))
    (if (or (equal? meta-store anc-meta-store)
	    (trivial-metastore? gen-meta-store))
	(let* ((substitutions
		(mk-substitution-from meta-store gen-meta-store))
	       (new-block
		(append substitutions
			`((goto ,gen-config))))
	       (new-node
		(mk-node #;config `(TRANSIT . ,config)
			 new-block
			 (if (equal? gen-meta-store anc-meta-store)
			     '()
			     (list (mk-open-node gen-config))))))
	  (substitute-in-tree config new-node tree))
	(let* ((substitutions
		(mk-substitution-from anc-meta-store gen-meta-store))
	       (new-block
		(append substitutions
			`((goto ,gen-config))))
	       (new-node
		(mk-node #;ancestor `(TRANSIT . ,ancestor)
			 new-block
			 `(,(mk-open-node gen-config)))))
	  (substitute-in-tree ancestor new-node tree)))))


;;; todo: does it make any difference if we first check for whistles and pick
;;; the closest one, or whether we pick the first superset, and if none is found
;;; then looking for [other] whistling configs? -> perhaps it does. both
;;; strategies seem to make sense, we'll see which one works better for DRC or sth.
;; possible other strategy:
;; instead of (first similar-ancestors) pick the one whose msg with leaf is the most specific....

(define (specialize tree block-map)
  (let drive ((tree tree))
;    (newline) (pretty-print tree)
    (let ((open-leaves (filter open-node? (get-leaves-of tree))))
      (if (null? open-leaves)
	  tree
	  (let* ((leaf (first open-leaves))
		 (config (get-label-of leaf))
		 (pp (get-pp-of-config config))
		 (meta-store (get-meta-store-of-config config))
		 (ancestor-configs
		  (reverse (filter (lambda (anc) (not (eq? (car anc) 'TRANSIT)))
			    (get-path-of config tree))))
		 (similar-ancestors
		  (filter (lambda (anc-config) (whistle? config anc-config))
			  ancestor-configs)))
	    (if (null? similar-ancestors)
		(drive (unfold-tree leaf tree block-map))
	      (let ((ancestor (first similar-ancestors)))
		(if (configuration-subset? config ancestor)
		    (drive (fold-tree leaf ancestor tree))
		    (drive (generalize-tree leaf ancestor tree))))))))))
