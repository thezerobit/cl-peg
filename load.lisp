(defpackage #:cl-peg
  (:use #:cl)
 )

(require :genhash)
(require :cl-ppcre)
(load "yacc.lisp")
(load "pegobjects.lisp")
(load "pegreader.lisp")
(load "matcher.lisp")
;(load "test.lisp")
