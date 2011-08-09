(in-package :asdf)

(defsystem :cost-calc
  :name "COST-CALC"
  :components ((:file "package")
	       (:file "$-calc")))