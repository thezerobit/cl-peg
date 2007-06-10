(in-package #:cl-peg)

;(declaim (optimize (speed 3) (safety 0) (debug 0)))

; ** classes modelling PEG expressions

(defclass parse-element () ((hash-val :type fixnum
				      :initform nil)))


(defclass grouping-element (parse-element) ((expr :initarg :expr :accessor expr)))

(defclass parse-element-with-slot (parse-element) ((feature :initarg :feature
							    :reader feature)))

(defclass magic-dot (parse-element-with-slot) ())
(defclass eof (parse-element-with-slot) ())
(defclass trivial-match (parse-element-with-slot) ())
(defclass call-rule (parse-element-with-slot) ())
(defclass character-class (parse-element-with-slot) ())
(defclass quoted-char (parse-element-with-slot) ())
(defclass quoted-string (parse-element-with-slot) ()) 

(defclass lambda-ref (parse-element-with-slot) ())
(defclass match (parse-element-with-slot) ())

; this is the only grouping-element with an extra slot

(defclass named-non-terminal (grouping-element) ((name :initarg :name :accessor name)))

(defclass grammar (grouping-element) ((non-terminal-map :initform nil
							:reader non-terminal-map)
				      (pe-map :initarg :pe-map 
					      :reader pe-map)
				      (ready-for-matching :initform nil
							  :reader ready-for-matching)
				      (parse-table :initform nil 
						   :reader parse-table)
))

(defclass expression-list (grouping-element) ((debugtag :initarg :debugtag)))
(defclass ordered-expr-list (grouping-element) ())
(defclass zero-or-more (grouping-element) ())
(defclass optional (grouping-element) ())
(defclass negated (grouping-element) ())
(defclass followed-by (grouping-element) ())
(defclass at-least-one (grouping-element) ())
(defclass bracketed-rule (grouping-element) ())



(defgeneric prepare-matching-structures (grammar))
(defgeneric clear-match-results (grammar))

; returns a pr struct

(defgeneric parse-and-match (parse-element input-char-list input-offset))


; printing peg objects

(defmethod print-object ((pe parse-element) stream)
  (format stream "PE: ~S ~S" (class-of pe) (slot-value pe 'hash-val))
)

(defmethod print-object ((nt named-non-terminal) stream)
  (format stream "NT: ~S ~S" (slot-value nt 'name) (slot-value nt 'hash-val))
)
; ** memoization 

; methods for comparing objects that we want to memoize

(defgeneric compare (a b))
(defgeneric hash (a))

(defun pe-compare (a b) 
  (if (not (eq a b))
      (cond ((not (eq (class-of a) (class-of b))) nil)
	    ((typep a 'parse-element) (compare a b))
	    ((typep a 'list) (compare a b))
	    (t (equal a b))
	    )
      t))
  
(defun pe-hash (a)
  (typecase a
    (parse-element (progn 
		     (if (null (slot-value a 'hash-val))
			 (setf (slot-value a 'hash-val) (hash a)))
		     (slot-value a 'hash-val)))
    (t (sxhash a))))


; this is the only way I could find to reset this hash-function
;(remhash 'cl-peg::pehash1 NET.HEXAPODIA.HASHTABLES::*HASH-NICKNAME-MAP*)
;FIXME
(NET.HEXAPODIA.HASHTABLES::register-hash-function 'pehash1 #'pe-hash #'pe-compare)

(defvar *pe-map*)
(setf *pe-map* (NET.HEXAPODIA.HASHTABLES::make-generic-hashtable :test 'pehash1))

(defun memoize (a) 
  (if (null (NET.HEXAPODIA.HASHTABLES::hashref a *pe-map*))
      (setf (NET.HEXAPODIA.HASHTABLES::hashref a *pe-map*) a))
  (if (null (NET.HEXAPODIA.HASHTABLES::hashref a *pe-map*))
      (break))
  (NET.HEXAPODIA.HASHTABLES::hashref a *pe-map*)
)

; this case should never occur, so this functions as an assertion
(defmethod compare ((a parse-element-with-slot) (b parse-element-with-slot))
  (if (eq (class-of a) (class-of b))
      (break "assertion failed")
      nil))

(defmethod compare ((a magic-dot) (b magic-dot))
  t)

(defmethod compare ((a eof) (b eof))
  t)

(defmethod compare ((a trivial-match) (b trivial-match))
  t)

(defmethod compare ((a call-rule) (b call-rule))
  (equal (slot-value a 'feature) (slot-value b 'feature)))
(defmethod compare ((a character-class) (b character-class))
  (equal (slot-value a 'feature) (slot-value b 'feature)))
(defmethod compare ((a quoted-char) (b quoted-char))
  (equal (slot-value a 'feature) (slot-value b 'feature)))
(defmethod compare ((a quoted-string) (b quoted-string))
  (equal (slot-value a 'feature) (slot-value b 'feature)))
(defmethod compare ((a match) (b match))
  (equal (slot-value a 'feature) (slot-value b 'feature)))


(defmethod hash ((pe parse-element-with-slot))
  (logxor (sxhash (class-of pe)) 
	  (sxhash (slot-value pe 'feature))))


; grouping elements and the rest

(defmethod hash ((ge grouping-element))
   (logxor (sxhash (class-of ge)) (hash (slot-value ge 'expr))))



(defmethod hash ((nt named-non-terminal))
  (prog1  (logxor (sxhash (class-of nt)) (sxhash (slot-value nt 'name)) (hash (slot-value nt 'expr)))
    ;    (break "nt ~A" nt)
    ))

(defmethod hash ((l list))
  (if (null l)
      (sxhash 'list)
      (loop for el in l and
	    accumulator = (sxhash 'list)
	    do (setf accumulator (logxor accumulator (hash el)))
	    finally ( return accumulator))))

(defmethod compare ((a null) (b null)) t)

(defmethod compare ((a t) (b t))
  (equal a b))

(defmethod compare ((a list) (b list))
      (if (compare (first a) (first b))
	  (compare (rest a) (rest b))
	  nil))

(defmethod compare ((a grouping-element) (b grouping-element))
;  (format t "~A is not ~A~%" a b)
  nil)

(defmethod compare ((a grammar) (b grammar))
  (eq a b))

(defmethod compare ((a expression-list) (b expression-list))
  (compare (slot-value a 'expr) (slot-value b 'expr)))

(defmethod compare ((a ordered-expr-list) (b ordered-expr-list))
  (compare (slot-value a 'expr) (slot-value b 'expr)))

(defmethod compare ((a zero-or-more) (b zero-or-more))
  (compare (slot-value a 'expr) (slot-value b 'expr)))

(defmethod compare ((a optional) (b optional))
  (compare (slot-value a 'expr) (slot-value b 'expr)))

(defmethod compare ((a negated) (b negated))
  (compare (slot-value a 'expr) (slot-value b 'expr)))

(defmethod compare ((a followed-by) (b followed-by))
  (compare (slot-value a 'expr) (slot-value b 'expr)))

(defmethod compare ((a at-least-one) (b at-least-one))
  (compare (slot-value a 'expr) (slot-value b 'expr)))

(defmethod compare ((a bracketed-rule) (b bracketed-rule))
  (compare (slot-value a 'expr) (slot-value b 'expr)))

(defmethod compare ((a named-non-terminal) (b named-non-terminal))
  (if (equal (slot-value a 'name) (slot-value b 'name))
      (compare (slot-value a 'expr) (slot-value b 'expr))
      nil))

(defclass parse-table () ((expressionToRowMap :initarg :expressionToRowMap
					      :reader expressionToRowMap
					      :type NET.HEXAPODIA.HASHTABLES::hash-container
			  )))


(defun make-parse-table () 
       (make-instance 'parse-table 
		      :expressionToRowMap (NET.HEXAPODIA.HASHTABLES::make-generic-hashtable :size 50000 :test 'pehash1)
		      )
)

(defvar *parse-result-table*)

; we set safety to 1 to be able to catch the invalid array reference
; I don't know if this is portable to lisps besides SBCL

; we have a hard-coded initial input length of 1000 characters
; TODO: expand-array should double the array length or increase it in some other way

(defun lookup-parse-result (pe-or-l input-offset) (declare (type (or parse-element list) pe-or-l ) (fixnum input-offset))
       (declare (optimize (speed 3) (safety 1) (debug 0)))
 (let 
     ((lookup 
		(NET.HEXAPODIA.HASHTABLES::hashref pe-or-l (expressionToRowMap *parse-result-table*))))
   (if (null lookup)
     nil
     (handler-case 
	 (let ((lookup2 (aref (the simple-vector lookup) input-offset)))
	   (if (eq lookup2 0)
	       nil
	       lookup2))
       (type-error () (progn 
			(setf (NET.HEXAPODIA.HASHTABLES::hashref pe-or-l (expressionToRowMap *parse-result-table*)) (expand-array lookup))
			nil)))
     
     ))
 )

; is there a better way for copying arrays?

(defun expand-array (array) (declare ((or nil simple-vector) array))
  (let ((temp (make-array 1000 :element-type t :initial-element nil)))
    (declare (simple-vector temp))
    (dotimes (i (length array))
      (setf (aref temp i) (aref array i))
      )
    temp)
)


; some speed tests

;; (defvar *test-ht*)
;; (setf *test-ht* (NET.HEXAPODIA.HASHTABLES::make-generic-hashtable :size 50000 :test 'pehash1))

;; (defun constest (i) 
;;   (declare (optimize (speed 3) (safety 1) (debug 0)))
;;   (if (eq (NET.HEXAPODIA.HASHTABLES::hashref '(wombat squirrel mouse) *test-ht*) #\l)
;;       (format t "yowzer ~A " (cons 'b '(1 23)))
;;       )
;;   (if (eq (mod i 5000) 0)
;;       (setf (NET.HEXAPODIA.HASHTABLES::hashref '(wombat squirrel mouse) *test-ht*) i))
;; )

;; (defun test-hash ()
;;   (memoize (make-instance 'call-rule :feature '|wosina|))
;;   (memoize (make-instance 'call-rule :feature '|wosina|))
;;   (memoize (make-instance 'call-rule :feature '|wosina|))
;; )


; the initial array length is the max of 1000 or the input-offset + 1

(defun set-parse-result (pe-or-l input-offset pr) (declare (type (or parse-element list) pe-or-l ) (type fixnum input-offset))
       (declare (optimize (speed 3) (safety 1) (debug 0)))
       (let ((lookup (NET.HEXAPODIA.HASHTABLES::hashref pe-or-l (expressionToRowMap *parse-result-table*))))
	 (if (null lookup)
	     (setf (NET.HEXAPODIA.HASHTABLES::hashref pe-or-l (expressionToRowMap *parse-result-table*)) (make-array (max 1000 (+ 1 input-offset)) :element-type t :initial-element nil)))
	 (let ((lookup2 (NET.HEXAPODIA.HASHTABLES::hashref pe-or-l (expressionToRowMap *parse-result-table*))))
		 (setf (aref (the simple-vector lookup2) input-offset) pr))
	 )
)

; set up the non-terminal-map to map the non-terminal name symbol to the named-non-terminal object
(defmethod prepare-matching-structures ((g grammar))
  (setf (slot-value g 'non-terminal-map) (make-hash-table))
  (loop for nt in (collect-non-terminals g) do 
	    (if (not (eq (gethash (slot-value nt 'name) (non-terminal-map g)) nil))
		(progn
		  (format t "~A" (list (slot-value nt 'name) " installing non-terminal twice!"))
		  (break "installing non-terminal ~A twice" (slot-value nt 'name))
		  )
		(setf (gethash (slot-value nt 'name) (non-terminal-map g)) nt)))
  ; initialize the parse-table
  (clear-match-results g)
  (setf (slot-value g 'ready-for-matching) t)
)

(defmethod clear-match-results ((g grammar))
  (setf (slot-value g 'parse-table) (make-parse-table))
)

(defun dump-pe-map (g)
  (NET.HEXAPODIA.HASHTABLES::all-hash-keys (pe-map g))
)

(defun workout ()
  (let ((a (make-instance 'optional :expr (list (make-instance 'zero-or-more :expr (list (make-instance 'quoted-string :feature "what"))))))
	(b (make-instance 'optional :expr (list (make-instance 'zero-or-more :expr (list (make-instance 'quoted-string :feature "gabble")))))))
    (print "a ")
    (print (hash a))
    (print "b ")
    (print (hash b))))

(defun workout2 ()
  (let ((a (make-instance 'optional :expr (list 
					   (make-instance 'call-rule :feature 'martin) 
					   (make-instance 'zero-or-more :expr (list 
									       (make-instance 'quoted-string :feature "what") 
									       (make-instance 'call-rule :feature 'martin))))))
	(b (make-instance 'optional :expr (list 
					   (make-instance 'call-rule :feature 'robert) 
					   (make-instance 'zero-or-more :expr (list 
									       (make-instance 'quoted-string :feature "what") 
									       (make-instance 'call-rule :feature 'robert)))))))
    (print "a ")
    (print (hash a))
    (print "b ")
    (print (hash b))))
  
