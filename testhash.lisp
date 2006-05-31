(use-package 'NET.HEXAPODIA.HASHTABLES)

(defvar *ht*)

(defun parsing-element-compare (a b)
  (if (eq a b)
      t
      (if (and (typep a 'parsing-element)
	       (typep b 'parsing-element))
	  (if (eql (type-of a) (type-of b))
	      (compare a b)
	      nil)
	  
  (if (equal (
)
(register-hash-function 'parsing-element #'sxhash #'parsing-element-compare)

(setf *ht* (make-generic-hashtable :test `