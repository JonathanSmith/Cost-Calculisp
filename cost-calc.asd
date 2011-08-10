(in-package :asdf)

(defsystem :cost-calc
    :name "cost-calc"
    :version "0.0.1"
    :components ((:module "source"
			  :components ((:file "defpackages")
				       (:file "cost-calc"
					      :depends-on
					      ("defpackages" "channels" "fibers" "utilities"))
				       (:file "channels")
				       (:file "fibers" :depends-on ("utilities" "channels"))
				       (:file "utilities")))))