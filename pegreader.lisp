(in-package #:cl-peg)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

; this file contains
;  some lexing routines to read the PEG grammar
;  the CL-YACC grammar definition and rules for constructing the grammar parse-tree

; lexing routines

; functions to turn a string into lexed structures

(defun str2list (str) 
  (loop for i across str collect i))

; loops across the input looking for the specified quote char
; (this function is called for each of [, " and ')
; when a quoteChar is encountered we slurp all the input up to the closing quote char
; and insert (lexSymbol string) into the result list

; I would love to find a more elegant way of doing this 

(defun prelex (charList quoteCharStart quoteCharEnd lexSymbol) 
  (let ((result ())
	(quotedChars ())
	(inString nil))
    (loop for c in charList
	  and prev-char = nil then c
	  do (progn 
	       (if (and (not (equal prev-char #\\)) 
			(or (equal c quoteCharStart) 
			    (equal c quoteCharEnd))) 
		   (progn (setq inString (not inString)) ;toggle whether we are inside the quote or not
			  (when (and quotedChars
				     (not inString))
			    (push (list lexSymbol (makeStringToken quotedChars)) result) 
			    (setq quotedChars nil)))
		   (if inString 
		       (if (equal prev-char #\\)
			   (progn
			     (pop quotedChars)
			     (push (case c
				     (#\t #\Tab)
				     (#\n #\Newline)
				     (otherwise c)) quotedChars))
			   (push c quotedChars))
		       (push c result)))))
    (nreverse result)))

; turn a list of chars into a string

(defun makeStringToken (l) 
  (cond
    ((null l) "")
    (t (concatenate 'string (makeStringToken  (rest l)) (list  (first l)) ))
    ))

; these characters are turned into lexical tokens

(defparameter operatorSubstitutions '((#\+ PLUS +) 
				      (#\* STAR *) 
				      (#\? QUESTION-MARK ?) 
				      (#\! EXCLAMATION-MARK !) 
				      (#\& AMPERSAND) 
				      (#\  SPACE) 
				      (#\( LEFT-BRACKET #\() 
				      (#\) RIGHT-BRACKET #\)) 
				      (#\/ SLASH /) 
				      (#\Newline NEWLINE NL)
				      (#\# HASH #\#)
				      (#\@ AT #\@)
				      (#\. MAGIC-DOT #\.)))
 
; strips out lines beginning with a semi-colon (;)
; perhaps we should also strip whitespace before the colon?

(defun strip-comments (str)
  (let ((output (make-string-output-stream))
	(input (make-string-input-stream str)))
    (do* ((scanner (cl-ppcre::create-scanner "^;.*"))
	  (line (read-line input nil) (read-line input nil)))
	 ((null line))
      (when (null (cl-ppcre::scan scanner line))
	(write-line line output)))
    (get-output-stream-string output))
)

; first deal with the quoting, then lex the operators and then process the LHS

(defun lex (str) 
  (let* ((preprocessed (strip-comments str))
	 (l (str2list preprocessed)))
    (makeIds (nsublis operatorSubstitutions 
		      (prelex 
		       (prelex 
			(prelex l #\[ #\] 'CHARACTER-CLASS)
			#\" #\" 'QUOTED-STRING)
		       #\' #\' 'QUOTED-CHAR)))))

; find the names left in the input and turn them into (ID-STRING ...) lists
; also process "<-" as (ASSIGN-TO)

; the repetition of code here is ugly

(defun makeIds (l) 
  (let ((result ()) 
	(buf ())) 
    (loop for item in l do 
	  (cond 
	    ((listp item) (if (first buf)
			      (progn 
				(if (equal (makeStringToken buf) "<-")
				    (progn 
				      (push (list 'ASSIGN-TO (makeStringToken buf)) result)
				      (setq buf nil))
				    (progn 
				      (push (list 'ID-STRING (makeStringToken buf)) result)
				      (setq buf nil)
				      )
				    )))
	     (if (not (equal (first item) 'SPACE)) (push item result)))
	    (t (push item buf)))
	  )
    (if (first buf) (progn 
		      (if (equal (makeStringToken buf) "<-")
			  (progn 
			    (push (list 'ASSIGN-TO (makeStringToken buf)) result)
			    (setq buf nil))
			  (progn 
			    (push (list 'ID-STRING (makeStringToken buf)) result)
			    (setq buf nil)
			    )
			  )))
    (nreverse result))
  ) 

; adapter to cl-yacc style lexer
; must return (values token value)
(defun peg-lexer (str) (let ((tokenlist (lex str)))
		      #'(lambda ()
			  (let ((value (pop tokenlist)))
			    (cond 
			      ((null value) (values nil nil))
			      (t (values (first value) (rest value)))
			      )
			    ))))

; strip a single-quote ', or a double quote " from a string
; there must be a better way to build the string?

(defun strip-quotes (str) 
  (let ((filtered-string (loop for c across str when (not (or (eq c #\') (eq c #\")))  collect c)))
    ;turn a list of chars into a string
    (let ((ns (make-string (length filtered-string))))
      (loop for i from 0 upto (- (length filtered-string) 1) do
	    (setf (char ns i) (nth i filtered-string)))
	ns)))

; rules for constructing the PEG grammar tree
; see pegobjects for definitions of the classes instantiated here

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun grammar (rss)
    (memoize (make-instance 'grammar :expr (memoize rss) :pe-map *pe-map*)))
  (defun ruleset (lhs assign-to ordered-expr-list) (declare (ignore assign-to))
	 (memoize (make-instance 'named-non-terminal :name (intern (strip-quotes (first lhs))) :expr ordered-expr-list)))
  (defun expression-list-with-hook (l hook)
    (if hook
	(if (typep l 'parse-element)
	    (memoize (list l (first hook)))
	    (memoize (append l hook)))
	(if (listp l)
        (memoize l)
        (memoize (list l)))))
    
  (defun expression-list2 (a b)
    (cond ((and (not (listp a))
		(not (listp b)))
	   (memoize (list a b)))
	  ((and (listp a)
		(listp b))
	   (memoize (append a b)))
	  ((listp a)
	   (memoize (append a (list b))))
	  ((listp b)
	   (memoize (append (list a) b)))
	  (t (error "bad expr case"))))
  (defun simple-ordered-expression-list (el)
    (memoize el))
  
  (defun ordered-expr-list (el oelt)
    (if (typep oelt 'ordered-expr-list)
        (memoize (make-instance 'ordered-expr-list :expr (memoize (append (list el) (slot-value oelt 'expr)))))
	(memoize (make-instance 'ordered-expr-list :expr (memoize (list el oelt))))))
  
  (defun tail-expression-list (slash el) (declare (ignore slash))
	 (memoize el))
  (defun character-class (cl)
    (memoize (make-instance 'character-class :feature (first cl) )))
  (defun quoted-char (qc)
    (memoize (make-instance 'quoted-char :feature (elt qc 0))))
  (defun quoted-string (qs)
    (let ((feature (first qs)))
      (if (> (length feature) 0)
	(memoize (make-instance 'quoted-string :feature (make-array (length feature) :element-type 'standard-char :initial-contents feature)))
; I am not sure if this is portable. I want to make sure the string is an array of standard-chars.; the simpler way: 
;	(memoize (make-instance 'quoted-string :feature (first qs)))
	(break "empty quoted string")
	)))
  (defun zero-or-more (e star) (declare (ignore star))
	 (memoize (make-instance 'zero-or-more :expr (memoize e))))
  (defun negated (neg e) (declare (ignore neg))
	 (memoize (make-instance 'negated :expr (memoize e))))
  (defun followed-by (amp e) (declare (ignore amp))
	 (memoize (make-instance 'followed-by :expr (memoize e))))
  (defun magic-dot (md) (declare (ignore md))
	 (memoize (make-instance 'magic-dot :feature #\.)))
  (defun eof (ex dot) (declare (ignore ex) (ignore dot))
	 (memoize (make-instance 'eof :feature 'EOF)))
    (defun at-least-one (e plus) (declare (ignore plus))
	 (memoize (make-instance 'at-least-one :expr (memoize e))))
  (defun optional (e qm) (declare (ignore qm))
	 (memoize (make-instance 'optional :expr (memoize e))))
  (defun bracketed-rule (a e b) (declare (ignore a b))
	 (cond ((and (listp e) 
		     (equal (list-length e) 1))
		(memoize (first e)))
	       (t (memoize e ))))
  (defun call-rule (i)
    (memoize (make-instance 'call-rule :feature (intern (first i)))))
  (defun nt1 (&rest words)
    (loop for w in words do 
	  (if t ; we can insert a user-supplied function here to remove extraneous words or characters in the LHS rules
	      (return-from nt1 w)))
    nil
    )
  (defun rulesets (ruleset nl rulesets) (declare (ignore nl))
	 (cond ((and (null ruleset) (null rulesets)) nil)
	       ((null ruleset) rulesets)
	       ((null rulesets) ruleset)
	       (t (cond ((listp rulesets) (memoize (cons ruleset rulesets)))
			(t (memoize (list ruleset rulesets)))))))
  (defun trivial-match (left-bracket right-bracket) (declare (ignore left-bracket right-bracket))
    (memoize (make-instance 'trivial-match :feature nil)))
  (defun lambda-ref (hash fun-name)
    (memoize (make-instance 'lambda-ref :feature (first fun-name))))
  (defun match(at fun-name)
    (memoize (make-instance 'match :feature (first fun-name))))
  )

; ** definition of PEG grammar

(cl-peg-yacc:define-parser *peg-grammar-parser* (:muffle-conflicts nil)
    (:start-symbol grammar)
  (:terminals (id-string ASSIGN-TO transient newline space norats slash ampersand question-mark exclamation-mark left-bracket right-bracket plus star character-class quoted-string quoted-char magic-dot at hash))
  (:precedence ((:left star plus question-mark slash) (:right ampersand exclamation-mark)))
  
  (grammar (rulesets #'grammar))
  (rulesets (ruleset newline rulesets #'rulesets) ())
  (ruleset (LHS assign-to ordered-expr-list #'ruleset) ())
  (LHS (id-string id-string id-string #'nt1) (id-string id-string #'nt1) (transient id-string #'nt1) (norats id-string #'nt1) (id-string #'nt1))
  (ordered-expr-list (expression-list #'simple-ordered-expression-list) (expression-list ordered-expr-list-tail #'ordered-expr-list))
  (ordered-expr-list-tail (slash ordered-expr-list #'tail-expression-list))
  (expression-list (expr action-hook #'expression-list-with-hook) (expr expression-list #'expression-list2))
  (action-hook (lambda-ref) ()) ;lambda-block
  (lambda-ref (hash id-string #'lambda-ref))
  (expr (bracketed-rule #'(lambda (pe) (memoize pe)))
	(exclamation-mark magic-dot #'eof) (expr star #'zero-or-more) (expr plus #'at-least-one) (expr question-mark #'optional) (ampersand expr #'followed-by) (exclamation-mark expr #'negated) (character-class #'character-class) (quoted-string #'quoted-string) (quoted-char #'quoted-char) (id-string #'call-rule) (at id-string #'match) (magic-dot #'magic-dot))
  (bracketed-rule (left-bracket ordered-expr-list right-bracket #'bracketed-rule) (left-bracket right-bracket #'trivial-match))
)

; method to parse a PEG file and return a grammar

(defun create-peg-parser (file-name) 
  (let ((morph_string  
	 (with-open-file (file file-name :direction :input) 
	   (let ((s (make-string (file-length file))))
	     (read-sequence s file) s))
	  )) 
    (NET.HEXAPODIA.HASHTABLES:hashclr *pe-map*)
    (cl-peg-yacc:parse-with-lexer (peg-lexer morph_string) *peg-grammar-parser*)))



(defun collect-non-terminals (grammar)
  (slot-value grammar 'expr))

(defun find-non-terminal (grammar name) 
  (loop for nt in (collect-non-terminals grammar) thereis 
	(if (equal name (slot-value nt 'name))
	    nt
	    nil)))


