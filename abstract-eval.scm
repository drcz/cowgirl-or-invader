;;; "don't belive A.E., see for yourself the summer fields"

;;; abstract-eval : sex x metastore -> meta-sex
(define (abstract-eval expr meta-store)
  (let AE ((expr expr))
    (match expr
      [(? ground-sex? g) (sex->metasex g)]
      [(? var? v) (lookup v meta-store)]
      [(op e)       
       (match `(,op ,(AE e))
	 [('car ('cons h t)) h]
	 [('cdr ('cons h t)) t]
	 [('car ('KONST _)) (error `(a-e error: ,expr))]
	 [('cdr ('KONST _)) (error `(a-e error: ,expr))]
	 [('atom? ('cons h t)) '(KONST ())]
	 [('atom? ('KONST k)) '(KONST T)]
	 [('atom? ('+ a b)) '(KONST T)]
	 [('atom? ('- a b)) '(KONST T)]
	 [('atom? ('* a b)) '(KONST T)]
	 [('atom? ('/ a b)) '(KONST T)]
	 [('atom? ('% a b)) '(KONST T)]
	 [('atom? ('= a b)) '(KONST T)]
	 [('atom? ('< a b)) '(KONST T)]
	 [('number? ('KONST k)) `(KONST ,(bool->T/nil (number? k)))]
	 [('number? ('cons h t)) '(KONST ())]
	 [('number? ('+ a b)) '(KONST T)]
	 [('number? ('- a b)) '(KONST T)]
	 [('number? ('* a b)) '(KONST T)]
	 [('number? ('/ a b)) '(KONST T)]
	 [('number? ('% a b)) '(KONST T)]
	 [otherwise otherwise])]
      [(op e1 e2)
       (match `(,op ,(AE e1) ,(AE e2))
	 [('+ ('KONST a) ('KONST b)) `(KONST ,(+ a b))]
	 [('- ('KONST a) ('KONST b)) `(KONST ,(- a b))]
	 [('* ('KONST a) ('KONST b)) `(KONST ,(* a b))]
	 [('/ ('KONST a) ('KONST b)) `(KONST ,(/ a b))] ; conv. to float?
	 [('% ('KONST a) ('KONST b)) `(KONST ,(modulo a b))]
	 [('= ('KONST a) ('KONST b)) `(KONST ,(bool->T/nil (eqv? a b)))]
	 [('< ('KONST a) ('KONST b)) `(KONST ,(bool->T/nil (< a b)))]
	 [('+ ('KONST 0) e) e]
	 [('+ e ('KONST 0)) e]
	 [('- e ('KONST 0)) e]
	 [('- e e) `(KONST 0)]
	 [('* ('KONST 1) e) e]
	 [('* e ('KONST 1)) e]
	 [('* ('KONST 0) e) `(KONST 0)]
	 [('* e ('KONST 0)) `(KONST 0)]
	 [('/ e 1) e]
	 [('/ e e) `(KONST 1)]
	 [('/ ('KONST 0) e) `(KONST 0)]
	 [('% e 1) e]
	 [('% ('KONST 0) e) `(KONST 0)]
	 [('= e ('cons h t)) `(KONST ())] ;; = works only on atoms, sir.
	 [('= ('cons h t) e) `(KONST ())]
	 ;;; NB do not add simplifications in here!
	 ;;; like (- (- (cv 1) (k 1)) (k 1)) -> (- (cv 1) (k 2))
	 ;;; or even [('+ e e) `(* ,e (KONST 2))]
	 ;;; because "should-be-generalized?" would not recognize them.
	 ;;; only the ones leading to KONST are ok.
	 ;;; and perhaps the ones with neutral element...
	 [otherwise otherwise])]
      [otherwise 'ERROR]))) ;; todo: nice error quit sth
