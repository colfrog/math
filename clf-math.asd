(asdf:defsystem clf-math
  :version "1.0.0"
  :author "Laurent Cimon <laurent@nilio.ca>"
  :maintainer "Laurent Cimon <laurent@nilio.ca>"
  :license "bsd-2-clause"
  :description "Personal math library made with love for fun"
  :components ((:file "package")
	       (:file "3d-vector")
	       (:file "quaternion")
	       (:file "discrete")
	       (:file "rsa")
	       (:file "dyn-array")
	       (:file "stack")
	       (:file "queue")
	       (:file "heap")
	       (:file "btree")
	       (:file "graph")
	       (:file "disp-table")
	       (:file "weighted-graph")
	       (:file "probabilite-discrete")
	       (:file "probabilite-continue")))
