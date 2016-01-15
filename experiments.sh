#!/bin/sh

echo "power, m=5, n=3"
(cat experiments/power.fcl ; echo "((m . (KONST 5)) (n . (KONST 3)))")  | guile cowgirl.scm
echo ""
echo "power, m=?, n=3"
(cat experiments/power.fcl ; echo "((m . (CVAR 0)) (n . (KONST 3)))")  | guile cowgirl.scm
echo ""
echo "power, m=23, n=?"
(cat experiments/power.fcl ; echo "((m . (KONST 23)) (n . (CVAR 0)))")  | guile cowgirl.scm
echo ""
echo "power, m=?, n=?"
(cat experiments/power.fcl ; echo "((m . (CVAR 0)) (n . (CVAR 1)))")  | guile cowgirl.scm
echo ""
echo "-----------------------"
echo "map-from-cmp"
echo "n=3"
(cat experiments/map-from-comp.fcl ; echo "((n . (KONST 3)))" ) | guile cowgirl.scm
echo ""
echo "n=?"
(cat experiments/map-from-comp.fcl ; echo "((n . (CVAR 0)))" ) | guile cowgirl.scm
echo ""
echo "-----------------------"
echo "a-list"
echo "k='y al=((x.?) (y.?))"
(cat experiments/a-list.fcl ; echo "((k . (KONST y)) (al . (cons (cons (KONST x) (CVAR 0)) (cons (cons (KONST y) (CVAR 1)) (KONST ())))))") | guile cowgirl.scm
echo ""
echo "-----------------------"
echo ""
echo "append"
echo "xs='(q w), ys=?"
(cat experiments/apd.fcl ; echo "((xs . (cons (KONST q) (cons (KONST w) (KONST ())))) (ys . (CVAR 0)))")  | guile cowgirl.scm
echo ""
echo "-----------------------"
echo ""
echo "eval-expr"
echo "expr='(+ 3 x), store=[(x->x0)]"
(cat experiments/eval-expr.fcl ; echo "((expr . (cons (KONST +) (cons (KONST 3) (cons (KONST x) (KONST ()))))) (store . (cons (cons (KONST x) (CVAR 0)) (KONST ())))))" ) | guile cowgirl.scm
echo ""
echo "-----------------------"
echo ""
echo "eval-expr"
echo "expr='(- (* x x) 1), store=[(x->x0)]"
(cat experiments/eval-expr.fcl ; echo "((expr . (cons (KONST -) (cons (cons (KONST *) (cons (KONST x) (cons (KONST x) (KONST ())))) (cons (KONST 1) (KONST ()))))) (store . (cons (cons (KONST x) (CVAR 0)) (KONST ()))))" ) | guile cowgirl.scm
echo ""
echo "-----------------------"
echo ""
echo "length"
echo "xs='(q ? ?)"
(cat experiments/length.fcl ; echo "((xs . (cons (KONST q) (cons (CVAR 0) (cons (CVAR 1) (KONST ()))))))") | guile cowgirl.scm 
echo ""
echo "-----------------------"
echo ""
echo "length"
echo "xs='(q ? ? . ?)"
(cat experiments/length.fcl ; echo "((xs . (cons (KONST q) (cons (KONST w) (cons (KONST e) (CVAR 0))))))") | guile cowgirl.scm 
echo ""
echo "-----------------------"
echo ""

echo ""
echo "[drcz2->fcl '((! mk-adder (^ (x) (^ (y) (+ x y)))) (! main (^ (x0) ((mk-adder 3) x0))))] :"
echo "x0 = ?"
(cat experiments/prg0.fcl ; echo "((x0 . (CVAR 0)))")  | guile cowgirl.scm
echo ""
echo "-----------------------"
echo ""
echo "[drcz2->fcl '((! map (^ (f l) (if (= l ()) () (cons (f (car l)) (map f (cdr l)))))) (! main (^ (x0) (map (^ (x) (* x x)) x0))))] :"
echo "x0 = '(? ?)"
(cat experiments/prg3.fcl ; echo "((x0 . (cons (CVAR 0) (cons (CVAR 1) (KONST ())))))")  | guile cowgirl.scm
echo ""
echo "-----------------------"
echo ""
echo "[drcz2->fcl '((! sp (^ (xs ys) (if (= xs ()) () (cons (* (car xs) (car ys)) (sp (cdr xs) (cdr ys)))))) (! main (^ (xs) (sp xs '(1 2 -3))))] : "
echo "xs = '(? ? ?)"
(cat experiments/sp.fcl ; echo "((xs . (cons (CVAR 0) (cons (CVAR 1) (cons (CVAR 2) (KONST ()))))))") | guile cowgirl.scm
echo ""
echo "-----------------------"
echo ""
echo "[drcz2->fcl '[ (! mk-adder (^ (x) (^ (y) (+ x y)))) (! map (^ (f l) (if (= l ()) () (cons (f (car l)) (map f (cdr l))))))
  (! main (^ (xs) (map (mk-adder 3) xs))) ]"
echo "xs = '(2 ? ?)"
(cat experiments/map-adder.fcl ; echo "((xs . (cons (KONST 2) (cons (CVAR 0) (cons (CVAR 1) (KONST ()))))))") | guile cowgirl.scm 
