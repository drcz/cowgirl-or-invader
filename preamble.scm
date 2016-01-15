;;; TODO: tu dodac opisy co jest co
;;; TODO: dodać też nieużywane w kodzie definicje typów, dla czytającego i dla testów poligonowych, np cvar?, pp?, meta-store? itd

(define (error msg) (pretty-print msg) (quit)) ;; hehe.

(define (var? x) (symbol? x))
(define (atom? e) (not (pair? e)))

(define (bool->T/nil b) (if b 'T '()))

(define (ground-sex? e)
  (or (null? e)
      (eq? e 'T)
      (number? e)
      (and (pair? e) (eq? (car e) 'quote))))

(define (mk-config pp meta-store) (cons pp meta-store))
(define (get-pp-of-config config) (car config))
(define (get-meta-store-of-config config) (cdr config))

(define (konst? me) (and (pair? me) (eq? (car me) 'KONST)))

(define (all-cvars-in meta-sex)
  (let A ((desc meta-sex))
    (match desc      
      [('CVAR n) `((CVAR ,n))]
      [(op h t) (append (A h) (A t))]
      ;; this one covers KONST but also lack of desc (cf fresh-mvar):
      [otherwise '()])))

(define (gather-all-cvars meta-store)
  (append-map all-cvars-in (map cdr meta-store)))


;;;; stores...

(define (lookup v store)
  (let ((val (assoc v store)))
    (if val (cdr val) #f)))

(define (update v e store)
  (match store    
    [() `((,v . ,e))]
    [((v0 . e0) . store)
     (if (eq? v0 v)
	 `((,v0 . ,e) . ,store)
	 `((,v0 . ,e0) . ,(update v e store)))]))

(define (store-drop v store)
  (match store
    [() '()]
    [((v0 . e0) . store) (if (eq? v0 v)
			     store
			     `((,v0 . ,e0) . ,(store-drop v store)))]))


;;; trees... -- TODO!!!

(define (mk-node label content children) (list label content children))
(define (get-label-of node) (car node))
(define (get-content-of node) (cadr node))
(define (get-children-of node) (caddr node))

(define (get-leaves-of tree) ;; flatten?
  (let traverse ((tree tree))
    (let ((children (get-children-of tree)))
      (if (null? children)
	  (list tree)
	  (append-map traverse children)))))

(define (get-path-of node-label tree)
  (let traverse ((tree tree)
		 (path '()))
    (let ((label (get-label-of tree))
	  (children (get-children-of tree)))
      (if (equal? label node-label)
	  (reverse path)
	  (append-map (lambda (t) (traverse t (cons label path))) children)))))

(define (substitute-in-tree cut-label subtree tree)
  (let traverse ((tree tree))
    (let ((label (get-label-of tree))
	  (content (get-content-of tree))
	  (children (get-children-of tree)))
       (if (equal? label cut-label)
	   subtree
	   `(,label ,content ,(map traverse children))))))

