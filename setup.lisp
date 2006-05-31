(defvar *lg*)
(setf *lg* (create-peg-parser "/home/jewel/scratch/pegparser/lojban.peg.4"))
(setf *lg* (create-peg-parser "/home/jewel/dev/pegparser/grammar3.peg"))
(parse-and-match-special *lg* (find-non-terminal *lg* '|number|) "noroi gerku" 0)

(in-package :cl-peg)
(defvar *lg*)
(setf *lg* (create-peg-parser "/home/jewel/dev/pegparser/grammar3.peg"))

(defvar *lg*)
(setf *lg* (cl-peg:create-peg-parser "/home/jewel/dev/lojban_peg/lojban_grammar.peg"))

(cl-peg:p *lg* "ko sipna")

(sb-sprof:start-profiling)
(p " i lu ei do ckeji sei la alis cusku i do barda nixli tosa'a ju'o drani toi i i'enai ca'o tai klaku i le'o ko ca sisti li'u i ku'i abu za'o klaku loi litce be li so'i ja'e le nu abu se sruri lo barda lalxu noi centre li ji'ipano le ka condi kei gi'e preja le xadba be le kumfa ")
(sb-sprof:stop-profiling)
(sb-sprof:report)