(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defvar *pr*)

(defun make-graph ()
  (let ((*pr* (cl-peg:pp *lg* "ko sipna" t)))
    (cl-dot:dot-graph (cl-dot:generate-graph (cl-peg:pr-result *pr*)) "mydot.ps"))
)

(defmethod cl-dot:object-node ((pv cl-peg:pv))
  (if (not (null pv))
      (let ((pe (cl-peg:pv-parse-element pv)))
	(if (typep pe (find-class 'cl-peg:named-non-terminal))
	    (if (eq (slot-value pe 'cl-peg::NAME) '|gismu|)
		(make-instance 'cl-dot:node :attributes (list :label (concatenate 'string "gismu: " (cl-peg:pv-input-string *pr* pv ))))
		(make-instance 'cl-dot:node :attributes (list :label (string (slot-value pe 'cl-peg::NAME))))
		)
	    )
	)))

(defmethod cl-dot:object-node ((l list))
  (make-instance 'cl-dot:node :attributes (list :label ".")))

(defmethod cl-dot:object-points-to ((l list))
  (reverse l)
)

(defmethod cl-dot:object-points-to ((pv cl-peg:pv))
  (if (not (listp (cl-peg:pv-children pv)))
      (list (cl-peg:pv-children pv))
      (reverse (cl-peg:pv-children pv)))
)

(defmethod cl-dot:object-pointed-to-by ((pv cl-peg:pv))
)

(defmethod cl-dot:object-knows-of ((pv cl-peg:pv))
 ; (format t "~A knows ~A" pv (cl-peg:pv-children pv))
;  (cl-peg:pv-children pv)
)

(defmethod cl-dot:object-node ((pr cl-peg:pr))
  (cl-peg:pr-result pr)
)