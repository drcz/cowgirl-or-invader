# cowgirl-or-invader

An online partial evaluator for FCL with partially static datastructures.

This is a prototype. it has at least some termination issues yet to be repaired/redesigned.
Runs with guile-2.0, compiles with gambit-c.


# example use:

FCL program computing (expt m n):
 cat experiments/power.fcl

propagate both input values (m=5, n=3):
 (cat experiments/power.fcl ; echo "((m . (KONST 5)) (n . (KONST 3)))")  | guile cowgirl.scm

propagate the exponent (m=?, n=3):
 (cat experiments/power.fcl ; echo "((m . (CVAR 0)) (n . (KONST 3)))")  | guile cowgirl.scm

propagate the base (m=23, n=3):
 (cat experiments/power.fcl ; echo "((m . (KONST 23)) (n . (CVAR 0)))")  | guile cowgirl.scm


See more experiments in experiment.sh.



# todo:
-- rewrite TEST-configurations.scm,
-- fixing generalization (driving does not terminate on certain programs, like experiments/prg2.fcl),
-- perhaps "stop branch on impossible operation" -- eg if AE gets "(car ())", assuming the program is correct,
   means that current branch is not a possible computation,
-- cleanup,
...
