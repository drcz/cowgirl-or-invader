all:	cowgirl

cowgirl:	abstract-eval.scm configurations.scm cowgirl.scm drive.scm finalize.scm match.scm meta-sex.scm preamble.scm srfi-excerpts.scm
	/usr/local/Gambit-C/bin/gsc -exe -e '(include "/usr/local/Gambit-C/lib/syntax-case.scm")' cowgirl.scm
