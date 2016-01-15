(define-syntax let-string-start+end
  (syntax-rules ()
    ((let-string-start+end (start end) proc s-exp args-exp body ...)
     (receive (start end) (string-parse-final-start+end proc s-exp args-exp)
       body ...))
    ((let-string-start+end (start end rest) proc s-exp args-exp body ...)
     (receive (rest start end) (string-parse-start+end proc s-exp args-exp)
       body ...))))

;;; This one parses out a *pair* of final start/end indices. 
;;; Not exported; for internal use.
(define-syntax let-string-start+end2
  (syntax-rules ()
    ((l-s-s+e2 (start1 end1 start2 end2) proc s1 s2 args body ...)
     (let ((procv proc)) ; Make sure PROC is only evaluated once.
       (let-string-start+end (start1 end1 rest) procv s1 args
         (let-string-start+end (start2 end2) procv s2 rest
           body ...))))))

(define-syntax receive 
  (syntax-rules ()
    ((_ args expr body ...)
     (call-with-values (lambda () expr) (lambda args body ...)))))

(define (string-contains super sub)
  (let ((l1 (string-length super))
	(l2 (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i l2) l1)
	     #f)
	    ((string=? (substring super i (+ i l2)) sub)
	     i)
	    (else 
	     (loop (+ i 1)))))))

(define (string-contains-ci super sub)
  (let ((l1 (string-length super))
	(l2 (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i l2) l1)
	     #f)
	    ((string-ci=? (substring super i (+ i l2)) sub)
	     i)
	    (else 
	     (loop (+ i 1)))))))


(define (string-prefix? s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2) 
			 string-prefix? s1 s2 maybe-starts+ends
    (%string-prefix? s1 start1 end1 s2 start2 end2)))

(define (string-suffix? s1 s2 . maybe-starts+ends)
  (let-string-start+end2 (start1 end1 start2 end2) 
			 string-suffix? s1 s2 maybe-starts+ends
    (%string-suffix? s1 start1 end1 s2 start2 end2)))

(define (%string-prefix? s1 start1 end1 s2 start2 end2)
  (let ((len1 (- end1 start1)))
    (and (<= len1 (- end2 start2))	; Quick check
	 (= (%string-prefix-length s1 start1 end1
				   s2 start2 end2)
	    len1))))

(define (%string-suffix? s1 start1 end1 s2 start2 end2)
  (let ((len1 (- end1 start1)))
    (and (<= len1 (- end2 start2))	; Quick check
	 (= len1 (%string-suffix-length s1 start1 end1
					s2 start2 end2)))))

(define (%string-prefix-length s1 start1 end1 s2 start2 end2)
  (let* ((delta (min (- end1 start1) (- end2 start2)))
	 (end1 (+ start1 delta)))

    (if (and (eq? s1 s2) (= start1 start2))	; EQ fast path
	delta

	(let lp ((i start1) (j start2))		; Regular path
	  (if (or (>= i end1)
		  (not (char=? (string-ref s1 i)
			       (string-ref s2 j))))
	      (- i start1)
	      (lp (+ i 1) (+ j 1)))))))

(define (%string-suffix-length s1 start1 end1 s2 start2 end2)
  (let* ((delta (min (- end1 start1) (- end2 start2)))
	 (start1 (- end1 delta)))

    (if (and (eq? s1 s2) (= end1 end2))		; EQ fast path
	delta

	(let lp ((i (- end1 1)) (j (- end2 1)))	; Regular path
	  (if (or (< i start1)
		  (not (char=? (string-ref s1 i)
			       (string-ref s2 j))))
	      (- (- end1 i) 1)
	      (lp (- i 1) (- j 1)))))))

(define (string-parse-final-start+end proc s args)
  (receive (rest start end) (string-parse-start+end proc s args)
    (if (pair? rest) (error "Extra arguments to procedure" proc rest)
	(values start end))))

(define (string-parse-start+end proc s args)
  (if (not (string? s)) (error "Non-string value" proc s))
  (let ((slen (string-length s)))
    (if (pair? args)
	(let ((start (car args))
	      (args (cdr args)))
	  (if (and (integer? start) (exact? start) (>= start 0))
	      (receive (end args)
		  (if (pair? args)
		      (let ((end (car args))
			    (args (cdr args)))
			(if (and (integer? end) (exact? end) (<= end slen))
			    (values end args)
			    (error "Illegal substring END spec" proc end s)))
		      (values slen args))
		(if (<= start end) (values args start end)
		    (error "Illegal substring START/END spec"
			   proc start end s)))
	      (error "Illegal substring START spec" proc start s)))

	(values '() 0 slen))))


(define (%string-copy! to tstart from fstart fend)
  (if (> fstart tstart)
      (do ((i fstart (+ i 1))
	   (j tstart (+ j 1)))
	  ((>= i fend))
	(string-set! to j (string-ref from i)))
      (do ((i (- fend 1)                    (- i 1))
	   (j (+ -1 tstart (- fend fstart)) (- j 1)))
	  ((< i fstart))
	(string-set! to j (string-ref from i)))))

(define (string-concatenate strings)
  (let* ((total (do ((strings strings (cdr strings))
		     (i 0 (+ i (string-length (car strings)))))
		    ((not (pair? strings)) i)))
	 (ans (make-string total)))
    (let lp ((i 0) (strings strings))
      (if (pair? strings)
	  (let* ((s (car strings))
		 (slen (string-length s)))
	    (%string-copy! ans i s 0 slen)
	    (lp (+ i slen) (cdr strings)))))
    ans))

(define (string-join strings delim)
  (if (null? strings)
      ""
      (let ((buildit (lambda (lis final)
		       (let recur ((lis lis))
			 (if (pair? lis)
			     (cons delim (cons (car lis) (recur (cdr lis))))
			     final)))))
	(string-concatenate (cons (car strings) (buildit (cdr strings) '()))))))

(define (white-char? ch)
  (or (char=? ch #\space)
      (char=? ch #\newline)
      (char=? ch #\linefeed)
      (char=? ch #\return)
      (char=? ch #\tab)))

(define (string-trim-both str)
  (let* ((pos (let loop ((pos 0))
		(if (and (< pos (string-length str))
			 (white-char? (string-ref str pos)))
		    (loop (+ 1 pos))
		    pos)))
	 (str1 (substring str pos (string-length str)))
	 (pos (let loop ((pos (- (string-length str1) 1)))
		(if (and (>= pos 0)
			 (white-char? (string-ref str1 pos)))
		    (loop (- pos 1))
		    pos)))
	 (str2 (substring str1 0 (+ pos 1))))
    str2))

(define (car+cdr pair) (values (car pair) (cdr pair)))

(define first car)

(define (null-list? l)
  (cond ((pair? l) #f)
	((null? l) #t)
	(else (error "null-list?: argument out of domain" l))))

(define (%cars+cdrs lists)
  (call-with-current-continuation
    (lambda (abort)
      (let recur ((lists lists))
        (if (pair? lists)
	    (receive (list other-lists) (car+cdr lists)
	      (if (null-list? list) (abort '() '()) ; LIST is empty -- bail out
		  (receive (a d) (car+cdr list)
		    (receive (cars cdrs) (recur other-lists)
		      (values (cons a cars) (cons d cdrs))))))
	    (values '() '()))))))

(define (%cars+cdrs+ lists cars-final)
  (call-with-current-continuation
    (lambda (abort)
      (let recur ((lists lists))
        (if (pair? lists)
	    (receive (list other-lists) (car+cdr lists)
	      (if (null-list? list) (abort '() '()) ; LIST is empty -- bail out
		  (receive (a d) (car+cdr list)
		    (receive (cars cdrs) (recur other-lists)
		      (values (cons a cars) (cons d cdrs))))))
	    (values (list cars-final) '()))))))

(define (%cdrs lists)
  (call-with-current-continuation
    (lambda (abort)
      (let recur ((lists lists))
	(if (pair? lists)
	    (let ((lis (car lists)))
	      (if (null-list? lis) (abort '())
		  (cons (cdr lis) (recur (cdr lists)))))
	    '())))))

(define (%cars+ lists last-elt)	; (append! (map car lists) (list last-elt))
  (let recur ((lists lists))
    (if (pair? lists) (cons (caar lists) (recur (cdr lists))) (list last-elt))))

(define (every pred lis1 . lists)
  (if (pair? lists)
      ;; N-ary case
      (receive (heads tails) (%cars+cdrs (cons lis1 lists))
	(or (not (pair? heads))
	    (let lp ((heads heads) (tails tails))
	      (receive (next-heads next-tails) (%cars+cdrs tails)
		(if (pair? next-heads)
		    (and (apply pred heads) (lp next-heads next-tails))
		    (apply pred heads)))))) ; Last PRED app is tail call.
      ;; Fast path
      (or (null-list? lis1)
	  (let lp ((head (car lis1))  (tail (cdr lis1)))
	    (if (null-list? tail)
		(pred head)	; Last PRED app is tail call.
		(and (pred head) (lp (car tail) (cdr tail))))))))

(define (any pred lis1 . lists)
  (if (pair? lists)
      ;; N-ary case
      (receive (heads tails) (%cars+cdrs (cons lis1 lists))
	(and (pair? heads)
	     (let lp ((heads heads) (tails tails))
	       (receive (next-heads next-tails) (%cars+cdrs tails)
		 (if (pair? next-heads)
		     (or (apply pred heads) (lp next-heads next-tails))
		     (apply pred heads)))))) ; Last PRED app is tail call.
      ;; Fast path
      (and (not (null-list? lis1))
	   (let lp ((head (car lis1)) (tail (cdr lis1)))
	     (if (null-list? tail)
		 (pred head)		; Last PRED app is tail call.
		 (or (pred head) (lp (car tail) (cdr tail))))))))

(define (filter pred lis)			; Sleazing with EQ? makes this one faster
  (let recur ((lis lis))
    (if (null-list? lis) lis			; Use NOT-PAIR? to handle dotted lists.
	(let ((head (car lis))
	      (tail (cdr lis)))
	  (if (pred head)
	      (let ((new-tail (recur tail)))	; Replicate the RECUR call so
		(if (eq? tail new-tail) lis
		    (cons head new-tail)))
	      (recur tail))))))			; this one can be a tail call.

(define (fold kons knil lis1 . lists)
  (if (pair? lists)
      (let lp ((lists (cons lis1 lists)) (ans knil))	; N-ary case
	(receive (cars+ans cdrs) (%cars+cdrs+ lists ans)
	  (if (null? cars+ans) ans ; Done.
	      (lp cdrs (apply kons cars+ans)))))
      (let lp ((lis lis1) (ans knil))			; Fast path
	(if (null-list? lis) ans
	    (lp (cdr lis) (kons (car lis) ans))))))

(define (reduce f ridentity lis)
  (if (null-list? lis) ridentity
      (fold f (car lis) (cdr lis))))

(define (fold-right kons knil lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))		; N-ary case
	(let ((cdrs (%cdrs lists)))
	  (if (null? cdrs) knil
	      (apply kons (%cars+ lists (recur cdrs))))))
      (let recur ((lis lis1))				; Fast path
	(if (null-list? lis) knil
	    (let ((head (car lis)))
	      (kons head (recur (cdr lis))))))))

(define (delete x lis =)
  (filter (lambda (y) (not (= x y))) lis))

(define (delete-duplicates lis =)
  (let recur ((lis lis))
    (if (null-list? lis) lis
	(let* ((x (car lis))
	       (tail (cdr lis))
	       (new-tail (recur (delete x tail =))))
	  (if (eq? tail new-tail) lis (cons x new-tail))))))

(define (list-index pred lis1 . lists)
  (if (pair? lists)
      ;; N-ary case
      (let lp ((lists (cons lis1 lists)) (n 0))
	(receive (heads tails) (%cars+cdrs lists)
	  (and (pair? heads)
	       (if (apply pred heads) n
		   (lp tails (+ n 1))))))
      ;; Fast path
      (let lp ((lis lis1) (n 0))
	(and (not (null-list? lis))
	     (if (pred (car lis)) n (lp (cdr lis) (+ n 1)))))))

(define (drop lis k)
  (let iter ((lis lis) (k k))
    (if (zero? k) lis (iter (cdr lis) (- k 1)))))

(define (lset-uni-2 a b)
  (cond ((null? a) b)
	((member (car a) b) (lset-uni-2 (cdr a) b))
	(else (cons (car a) (lset-uni-2 (cdr a) b)))))


(define (lset-union = . lists)
  (reduce (lambda (lis ans)		; Compute ANS + LIS.
	    (cond ((null? lis) ans)	; Don't copy any lists
		  ((null? ans) lis) 	; if we don't have to.
		  ((eq? lis ans) ans)
		  (else
		   (fold (lambda (elt ans) (if (any (lambda (x) (= x elt)) ans)
					       ans
					       (cons elt ans)))
			 ans lis))))
	  '() lists))

(define (find-tail pred list)
  (let lp ((list list))
    (and (not (null-list? list))
	 (if (pred (car list)) list
	     (lp (cdr list))))))

(define (member= x lis =)
  (find-tail (lambda (y) (= x y)) lis))

(define (lset-difference = lis1 . lists)
  (let ((lists (filter pair? lists)))	; Throw out empty lists.
    (cond ((null? lists)     lis1)	; Short cut
	  ((memq lis1 lists) '())	; Short cut
	  (else (filter (lambda (x)
			  (every (lambda (lis) (not (member= x lis =)))
				 lists))
			lis1)))))

(define (%lset2<= = lis1 lis2) (every (lambda (x) (member= x lis2 =)) lis1))

(define (lset<= = . lists)
  (or (not (pair? lists)) ; 0-ary case
      (let lp ((s1 (car lists)) (rest (cdr lists)))
	(or (not (pair? rest))
	    (let ((s2 (car rest))  (rest (cdr rest)))
	      (and (or (eq? s2 s1); Fast path
		       (%lset2<= = s1 s2)) ; Real test
		   (lp s2 rest)))))))

(define (lset= = . lists)
  (or (not (pair? lists)) ; 0-ary case
      (let lp ((s1 (car lists)) (rest (cdr lists)))
	(or (not (pair? rest))
	    (let ((s2   (car rest))
		  (rest (cdr rest)))
	      (and (or (eq? s1 s2); Fast path
		       (and (%lset2<= = s1 s2) (%lset2<= = s2 s1))) ; Real test
		   (lp s2 rest)))))))


(define (append-map f . lists)
  (apply append (apply map f lists)))

(define (concatenate lol)
  (fold-right append '() lol))

;(define (last l) (car (reverse l))) ;;; pozdro997 -- LOL
(define (last-pair lis)
  ;(check-arg pair? lis last-pair)
  (let lp ((lis lis))
    (let ((tail (cdr lis)))
      (if (pair? tail) (lp tail) lis))))
;;; ^ nie wiem czy mi sie to bardziej podoba jak moj 997 lol...
(define (last lis) (car (last-pair lis)))

(define (alist-delete key alist =)
  (cond ((null? alist) '())
	((= key (caar alist)) (cdr alist))
	(else (cons (car alist) (alist-delete key (cdr alist) =)))))

;;; Map F across L, and save up all the non-false results.
(define (filter-map f lis1 . lists)
  ;(check-arg procedure? f filter-map)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))
	(receive (cars cdrs) (%cars+cdrs lists)
	  (if (pair? cars)
	      (cond ((apply f cars) => (lambda (x) (cons x (recur cdrs))))
		    (else (recur cdrs))) ; Tail call in this arm.
	      '())))
      
      ;; Fast path.
      (let recur ((lis lis1))
	(if (null-list? lis) lis
	    (let ((tail (recur (cdr lis))))
	      (cond ((f (car lis)) => (lambda (x) (cons x tail)))
		    (else tail)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (lset-intersection = lis1 . lists)
  (let ((lists (delete lis1 lists eq?))) ; Throw out any LIS1 vals.
    (cond ((any null-list? lists) '()); Short cut
	  ((null? lists)          lis1); Short cut
	    (else (filter (lambda (x)
			      (every (lambda (lis) (member= x lis =)) lists))
			  lis1)))))
