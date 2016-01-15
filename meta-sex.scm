;;; converting expressions to descriptions and back.

;;; notice that "sex" are the expressions "ready to be placed in code",
;;; in particular every symbol or list is quoted.

;;; ah, and meta-variables (CVAR _) and (TVAR _) are treated as valid variable names.
;;; we'll convert them into symbols after the whole code is generated.

;;; perhaps this one should be called metasex->code ?
(define (metasex->sex me)
  (match me
    [('CVAR n) me]
    [('TVAR n) me] ;; ?
    [('KONST ()) '()]
    [('KONST 'T) 'T]
    [('KONST (? number? n)) n]
    [('KONST e) `(quote ,e)]
    [(op me)
     `(,op ,(metasex->sex me))]
    [(op me1 me2)
     (let ((e1 (metasex->sex me1))
	   (e2 (metasex->sex me2))
	   (drop-quote (lambda (e)
			 (if (and (pair? e)
				  (eq? (car e) 'quote))
			     (cadr e)
			     e))))
       (if (and (ground-sex? e1)
		(ground-sex? e2)
		(eq? op 'cons))
	   `(quote ,(cons (drop-quote e1) (drop-quote e2)))
	   `(,op ,e1 ,e2)))]
    [otherwise (pretty-print `(metasex->sex err on ,me))]
))



;;; TODO -- czy tu jeszcze nie braknie coś coś?
(define (sex->metasex e)
  (match e
    [('CVAR n) e]
    [('TVAR n) e]
    [() '(KONST ())]
    ['T '(KONST T)]
    [(? number? n) `(KONST ,n)]
    [('quote e) ;; TODO: niezaquoteowane też może?
     (let T ((e e))
       (if (pair? e)
	   `(cons ,(T (car e)) ,(T (cdr e)))
	   `(KONST ,e)))]))

