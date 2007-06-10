(in-package :cl-peg)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

; this file contains
;  matching rules to match each PEG expression type
;  these rules generate a pr (parse result) struct (see below)

; a hash-table to lookup non-terminals
(defvar *nt-lookup* nil)

; include the parse-result or not
(defvar *keep-parse-result* nil)
(defvar *clean-parse-tree* nil)

(defun parse (grammar input-text &key (input-offset 0) save-parse-tree uncleaned-parse-tree) (declare (type grammar grammar) (sequence input-text) (fixnum input-offset))
       ; use the first rule from the grammar as the target
       (parse-rule
	grammar 
	(first (slot-value grammar 'expr)) 
	input-text 
	input-offset 
	save-parse-tree
	uncleaned-parse-tree))

(defun parse-file (grammar file-name &key (input-offset 0) save-parse-tree uncleaned-parse-tree) (declare (type grammar grammar) (fixnum input-offset))
       ; use the first rule from the grammar as the target
       (parse-rule
	grammar 
	(first (slot-value grammar 'expr))
	(with-open-file (file file-name :direction :input) 
	  (let ((s (make-string (file-length file))))
	    (read-sequence s file) s))
	input-offset 
	save-parse-tree
	uncleaned-parse-tree))

; match the given non terminal

(defun parse-rule 
    (grammar non-terminal input-char-list input-offset save-parse-tree uncleaned-parse-tree)
  (if (null (ready-for-matching grammar))
      (prepare-matching-structures grammar))
  (clear-match-results grammar)
  (let* ((*nt-lookup* (non-terminal-map grammar))
	 (*parse-result-table* (parse-table grammar))
	 (*keep-parse-result* save-parse-tree)
	 (pr (parse-and-match non-terminal input-char-list input-offset)))
    (progn 
      (setf (parse-result-original-input pr) input-char-list)
      (setf (parse-result-original-input-offset pr) input-offset)
      (setf (parse-result-matched-whole-input pr) (matched-all pr)))
      (if (and (parse-result-matched pr)
	       (not uncleaned-parse-tree))
	  (build-pv-result-tree pr)
	  pr
      )))


; each parsing element has a parse-and-match function
; the parse table stores a parse-node for each parse-element at each attempted input-offset


(defstruct (parse-node (:constructor make-parse-value (parse-element children start-offset end-offset))
	       )
  (parse-element nil :type (or parse-element list null))
  (children nil )
  (start-offset 0 :type fixnum)
  (end-offset 0 :type fixnum))

(defmethod print-object ((pv parse-node) stream)
  (format stream "#S(PV :PE ~A :CHILDREN ~A)" (parse-node-parse-element pv) (parse-node-children pv))
)

(defstruct (parse-result (:constructor make-pr (matched root-parse-node))
	       )
  (matched nil :type (or null t))
  (matched-whole-input nil :type (or null t))
  (root-parse-node nil :type (or null parse-node))
  (original-input nil)
  (original-input-offset 0 :type fixnum))

(defun pr-matched (pr) (parse-result-matched pr))
(defun pr-end-offset (pr) (parse-result-end-offset pr))
(defun pr-start-offset (pr) (parse-result-start-offset pr))

(defun parse-result-start-offset (pr)
  (parse-node-start-offset (parse-result-root-parse-node pr))
)

(defun parse-result-end-offset (pr)
  (parse-node-end-offset (parse-result-root-parse-node pr))
)

(defvar *parse-failure-sentinel* (make-pr nil nil))

(declaim (inline parse-fail))
(defun parse-fail () 
  *parse-failure-sentinel*
)

(defun save-pr (matched parse-element children start-offset end-offset) (declare (boolean matched) (fixnum start-offset) (fixnum end-offset))
  (if *keep-parse-result*
      (make-pr matched (make-parse-value parse-element children start-offset end-offset))
      (make-pr matched (make-parse-value nil nil start-offset end-offset))
))

; perhaps for portability we should not limit this to strings

(declaim (inline get-input-el))

(defun get-input-el (input-sequence offset) (declare (simple-string input-sequence) (fixnum offset))
       (if (>= offset (length input-sequence))
	   nil
	   (aref input-sequence offset))
)

(declaim (inline get-input-elements))

(defun get-input-elements (input-sequence start-offset end-offset) (declare (sequence input-sequence) (fixnum start-offset) (fixnum end-offset))
	 (if (or (>= start-offset (length input-sequence))
		 (> end-offset (length input-sequence)))
	     nil
	     (subseq input-sequence start-offset end-offset))
)

(defvar *input-string* nil)

(defun parse-node-input-string (pr pv)
  (subseq (parse-result-original-input pr) (parse-node-start-offset pv) (parse-node-end-offset pv))
)

(defun matched-region (pr)
  (list (parse-node-start-offset (parse-result-root-parse-node pr)) (parse-node-end-offset (parse-result-root-parse-node pr)))
)

(defun matched-all (pr)
  (if (pr-matched pr)
      (if (and (eq (first (matched-region pr)) (parse-result-original-input-offset pr))
	   (eq (first (rest (matched-region pr))) (length (parse-result-original-input pr))))
	  t
	  nil)
      nil))

(defun build-pv-result-tree (result)
  (if (null result)
      nil
      (typecase result
	(parse-node (let ((pe (parse-node-parse-element result)))
		      (if (typep pe (find-class 'named-non-terminal))
			  (progn 
			    (setf (parse-node-children result) (build-pv-result-tree (parse-node-children result)))
			    result)
			  (progn
			    (build-pv-result-tree (parse-node-children result)))
			  )))
	(list (let ((temp (loop for i in result when (build-pv-result-tree i) collect it)))
		(if (null (rest temp))
		    (first temp)
		    temp)))
	(parse-result (let ((tl (build-pv-result-tree (parse-result-root-parse-node result))))
			(progn
			  (if (not (listp tl))
			      (setf (parse-result-root-parse-node result) tl)
			      (setf (parse-result-root-parse-node result) (first tl)))
			  result)))
	 )))

; FIXME this is duplicated for parse-element and for list
; we check whether we have cached this lookup before doing the match itself

(defmethod parse-and-match :around ((pe parse-element) input-char-list (input-offset fixnum))
  (let ((pr (lookup-parse-result pe input-offset)))
    (if (null pr)
	(set-parse-result pe input-offset (call-next-method))
	pr))
)

(defmethod parse-and-match :around ((l list) input-char-list (input-offset fixnum))
  (let ((pr (lookup-parse-result l input-offset)))
    (if (null pr)
	(set-parse-result l input-offset (call-next-method))
	pr))
)

(defmethod parse-and-match ((g grouping-element) input-char-list input-offset)
   (parse-and-match (slot-value g 'expr) input-char-list input-offset)
)

(defmethod parse-and-match ((tr trivial-match) input-char-list input-offset)
  (save-pr t nil nil input-offset input-offset))

(defmethod parse-and-match ((f followed-by) input-char-list input-offset)
  (let ((pr (parse-and-match (slot-value f 'expr) input-char-list input-offset)))
    (if (parse-result-matched pr)
	(save-pr t nil nil input-offset input-offset)
	(parse-fail)
	)
    )
)

(defmethod parse-and-match ((n negated) input-char-list input-offset)
  (let ((pr (parse-and-match (slot-value n 'expr) input-char-list input-offset)))
    (if (not (parse-result-matched pr))
	(save-pr t nil nil input-offset input-offset)
	(progn
	  (parse-fail)))
    )
)

(defmethod parse-and-match ((alo at-least-one) input-char-list input-offset)
  (let ((pr (parse-and-match (slot-value alo 'expr) input-char-list input-offset)))
    (if (not (pr-matched pr))
	(parse-fail)
	(let ((pr2 (parse-and-match alo input-char-list (pr-end-offset pr))))
	  (if (pr-matched pr2)
	      (save-pr t alo (list (parse-result-root-parse-node pr) (parse-result-root-parse-node pr2)) (min (pr-start-offset pr) (pr-start-offset pr2)) (pr-end-offset pr2))
	      pr)
	  )
	)
    )
)


(defmethod parse-and-match ((opt optional) input-char-list input-offset)
  (let ((pr (parse-and-match (slot-value opt 'expr) input-char-list input-offset)))
    (if (pr-matched pr)
	pr
	(progn 
	  (save-pr t nil nil input-offset input-offset))
	)))

(defmethod parse-and-match ((zom zero-or-more) input-char-list input-offset)
  (let ((pr (parse-and-match (slot-value zom 'expr) input-char-list input-offset)))
    (if (not (pr-matched pr))
	(save-pr t nil nil input-offset input-offset)
	(let* ((pr2 (parse-and-match zom input-char-list (parse-node-end-offset (parse-result-root-parse-node pr))))
	       (pr-result2 (parse-result-root-parse-node pr2)))
	  (if (parse-result-matched pr2)
	      (save-pr t zom (list (parse-result-root-parse-node pr) pr-result2) (min (parse-node-start-offset pr-result2) (parse-node-start-offset pr-result2)) (parse-node-end-offset pr-result2))
	      pr
	  ))
	)
    ))

; not sure how to help the compiler make this efficient

(defmethod parse-and-match ((md magic-dot) input-char-list (input-offset fixnum))
  (let ((in-char (get-input-el input-char-list input-offset)))
    (if (eq (class-of in-char) (class-of #\b))
	(save-pr t md nil input-offset (+ (the (and fixnum (integer 0 1000000)) input-offset) 1))
	(parse-fail)
    )))

(defmethod parse-and-match ((eof eof) (input-char-list sequence) (input-offset fixnum))
  (if (>= input-offset (length input-char-list))
      (save-pr t eof nil input-offset input-offset)
      (parse-fail)
))

(defmethod parse-and-match ((q quoted-char) input-char-list (input-offset fixnum))
  (let ((in-char (get-input-el input-char-list input-offset))
	(feature-string (slot-value q 'feature)))
    (declare (simple-string feature-string))
    (let ((our-char (aref feature-string 0)))
        (if (eql in-char our-char)
	    (save-pr t q nil input-offset (+ input-offset 1))
	    (parse-fail))
	)))

; this is speed 1 because SBCL produces so many notes at speed 3
(defmethod parse-and-match ((q quoted-string) input-char-list (input-offset fixnum))
  (declare (optimize (speed 1)))
  (let* ((match-string (slot-value q 'feature))
	 (match-string-length (length match-string))
	 (input-new-offset (+ input-offset match-string-length))
	 (inp-string (get-input-elements input-char-list input-offset input-new-offset)))
    (if (equal match-string inp-string)
	  (save-pr t q nil input-offset input-new-offset)
	  (parse-fail)
	)
    )
  )

(defmethod parse-and-match ((cc character-class) input-char-list (input-offset fixnum))
  (let ((cin (get-input-el input-char-list input-offset)))
    (if (null cin)
	(parse-fail)
	(let ((c (the character cin)))
	  (loop for mchar of-type character across (the string (slot-value cc 'feature))
		do
		(progn
		  (if (eql c mchar)
		      (return-from parse-and-match (save-pr t cc nil input-offset (+ (the (and fixnum (integer 0 1000000)) input-offset) 1)))
		      )
		  )
		)
	  (parse-fail)
    ))))

(defmethod parse-and-match ((el expression-list) input-char-list input-offset)
  (parse-and-match (slot-value el 'expr) input-char-list input-offset))

(defmethod parse-and-match ((l list) input-char-list (input-offset fixnum))
  (if (null l)
      (break)
      (let ((pr (parse-and-match (first l) input-char-list input-offset)))
	(if (null (pr-matched pr))
	    (parse-fail)
	    (progn 
	      (if (null (rest l))
		  pr
		  (let ((pr2 (parse-and-match (rest l) input-char-list (parse-node-end-offset (parse-result-root-parse-node pr)))))
		    (if (not (null (pr-matched pr2)))
			(save-pr t l (list (parse-result-root-parse-node pr) (parse-result-root-parse-node pr2)) (min (parse-node-start-offset (parse-result-root-parse-node pr)) (parse-node-start-offset (parse-result-root-parse-node pr2))) (parse-node-end-offset (parse-result-root-parse-node pr2)))
			(parse-fail)
		  ))))))))

(defmethod parse-and-match ((oel ordered-expr-list) input-char-list input-offset)
  (let ((to-be-matched (slot-value oel 'expr)))
   (progn 
     (if (listp to-be-matched)
	 (loop for e in to-be-matched do 
	       (let ((pr (parse-and-match e input-char-list input-offset)))
		 (if (not (null (pr-matched pr)))
		     (progn
		       (return-from parse-and-match pr)
		     )
		     )
		 )
	       )
	 (parse-and-match to-be-matched input-char-list input-offset)
	 )
     (parse-fail)
     ))
)

(defmethod parse-and-match ((l lambda-ref) input-char-list input-offset)
  (save-pr t l nil input-offset input-offset))

(define-condition missing-non-terminal (error)
  ((text :initarg :text :reader text)))

(defmethod parse-and-match ((cr call-rule) input-char-list input-offset)
  (let ((nt (gethash (slot-value cr 'feature) *nt-lookup*)))
    (if (null nt)
	(progn
	  (format t "could not find non-terminal ~S"  (slot-value cr 'feature))
	  (error 'missing-non-terminal :text (format nil "could not find non-terminal ~S"  (slot-value cr 'feature)))
	  )
	(parse-and-match nt input-char-list input-offset)
	)
    )
)

(defmethod parse-and-match ((nt named-non-terminal) input-char-list input-offset)
  (let ((pr (parse-and-match (slot-value nt 'expr) input-char-list input-offset)))
    (if (not (null (pr-matched pr)))
	(save-pr t nt (list (parse-result-root-parse-node pr)) (pr-start-offset pr) (pr-end-offset pr))
	(parse-fail)
	)
    )
)

