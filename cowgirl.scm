;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        ;;;
;;;      C O W G I R L     ;;;
;;;           or           ;;;
;;;      I N V A D E R     ;;;
;;;                        ;;;
;;; 2016-01-03, drcz@o2.pl ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond-expand
 (gambit
  (include "match.scm")
  (include "srfi-excerpts.scm")
  (define quit exit)
  (define (iota n) (let loop ((i 0)) (if (< i n) (cons i (loop (+ i 1))) '()))))
 (guile-2
  (use-modules (ice-9 match)
               (ice-9 pretty-print)
               (srfi srfi-1))))

(include "preamble.scm")
(include "meta-sex.scm")
(include "abstract-eval.scm")
(include "configurations.scm")
(include "drive.scm")
(include "finalize.scm")

(let* ((pr (read)) ;; the code
       (mstore (read)) ;; initial store's description <- TODO maybe get just the descriptions for each input, hm?
       (init-pp (cadr pr))
       (blockmap (cddr pr))
       (process-tree (specialize #;drive
		       (mk-node (mk-config init-pp mstore) 'OPEN '())
		       blockmap))
       (new-blockmap (finalize process-tree))
       ;; NB assumes first block in new-blockmap is the root, ie you know, init.
       (new-init (caar new-blockmap))
       (new-input-vars (map mvar->symbol (gather-all-cvars mstore)))
       (new-program `(,new-input-vars
		      ,new-init
		      . ,new-blockmap)))
  (pretty-print new-program))

(quit)
