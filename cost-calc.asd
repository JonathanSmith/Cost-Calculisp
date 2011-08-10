(in-package :asdf)

(defsystem :cost-calc
    :name "cost-calc"
    :version "0.0.1"
    :components ((:module "src"
			  :components ((:file "defpackage")
				       (:file "cost-calc"
					      :depends-on
					      ("defpackage" "channels" "fibers" "utilities"))
				       (:file "channels")
				       (:file "fibers" :depends-on ("utilities" "channels"))
				       (:file "utilities")))))