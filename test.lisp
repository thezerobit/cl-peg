(require :cl-peg)

(defun test ()
  (let ((grammar (cl-peg:create-peg-parser "example.peg")))
    (cl-peg:parse grammar "the two dogs shot at the cannon" :save-parse-tree t)))