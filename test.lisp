(require :cl-peg)

(declaim (optimize (debug 3)))

(defvar *testg*)
(setf *testg* (cl-peg:create-peg-parser "testg.peg"))

(defun run-test ()
  (progn
    (if (not (cl-peg:find-non-terminal *testg* '|char|))
	(break))
    (if (not (cl-peg:find-non-terminal *testg* '|simple-ex-list|))
	(break))
    (if (not (cl-peg:parse-result-matched (cl-peg:parse-rule *testg* (cl-peg:find-non-terminal *testg* '|char|) "m" 0)))
	(break))
    (if (not (cl-peg:parse-result-matched (cl-peg:parse-rule *testg* (cl-peg:find-non-terminal *testg* '|char-class|) "e" 0)))
	(break))
    (if (not (cl-peg:parse-result-matched (cl-peg:parse-rule *testg* (cl-peg:find-non-terminal *testg* '|string|) "kik" 0)))
	(break))
    (if (not (cl-peg:parse-result-matched (cl-peg:parse-rule *testg* (cl-peg:find-non-terminal *testg* '|call-nt|) "m" 0)))
	(break))
    (if (not (cl-peg:parse-result-matched (cl-peg:parse-rule *testg* (cl-peg:find-non-terminal *testg* '|simple-ex-list|) "mukik" 0)))
	(break))
    (if (not (cl-peg:parse-result-matched (cl-peg:parse-rule *testg* (cl-peg:find-non-terminal *testg* '|mysentence|) "kikkikkikaeioummwomblez" 0)))
	(break))
    (if (not (cl-peg:parse-result-matched (cl-peg:parse-rule *testg* (cl-peg:find-non-terminal *testg* '|ord|) "f" 0)))
	(break))
    ; test an ordered expr list in brackets
    (if (not (cl-peg:parse-result-matched (cl-peg:parse-rule *testg* (cl-peg:find-non-terminal *testg* '|ord-in-brackets|) "m*" 0)))
	(break))
    (if (not (cl-peg:parse-result-matched (cl-peg:parse-rule *testg* (cl-peg:find-non-terminal *testg* '|ord-in-brackets|) "ed*" 0)))
	(break))
    (if (not (cl-peg:parse-result-matched (cl-peg:parse-rule *testg* (cl-peg:find-non-terminal *testg* '|ord-in-brackets|) "e    *" 0)))
	(break))
    (if (not (cl-peg:parse-result-matched (cl-peg:parse-rule *testg* (cl-peg:find-non-terminal *testg* '|follow-me|) "cddcfackik    *" 0)))
	(break))

    (format t "tests done")
    t
    )
)


	    
	    