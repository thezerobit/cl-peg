(defsystem cl-peg
  :description "PEG parser"
  :version "0.03"
  :author "John Leuner"
  :licence "MIT License"
  :components (	(:file "package")
		(:file "yacc" :depends-on ("package"))
               (:file "pegobjects" :depends-on ("yacc"))
               (:file "pegreader" :depends-on ("pegobjects"))
               (:file "matcher" :depends-on ("pegreader"))
	)
  :depends-on (cl-ppcre genhash))


