(in-package "COST-CALC")

(defstruct point 
  (x)
  (y))

(defgeneric x (point))
(defgeneric y (point))

(defmethod x ((point point))
  (point-x point))
(defmethod y ((point point))
  (point-y point))

(defclass TSP-NODE ($node)
  ((name :accessor node-name :initarg :name)
   (point :accessor point :initarg :point)))

(defparameter *all-nodes-hash* (make-hash-table))
(defparameter *all-nodes-list* nil)

(defmacro location (name x y)
  (let ((point (gensym "point")))
    `(eval-when (:load-toplevel)
       (let* ((,point (make-point :x ,x :y ,y)))
	 (push ',name *all-nodes-list*)
	 (setf (gethash ',name *all-nodes-hash*) ,point)))))

(defun get-loc (name)
  (gethash name *all-nodes-hash*))

(defmacro square (num)
  (let ((gnum (gensym "num")))
    `(let ((,gnum ,num))
       (* ,gnum ,gnum))))

(defun point-distance (np gp)
  (sqrt (+ (square (- (x np) (x gp))) (square (- (y np) (y gp))))))

(defmethod distance ((node TSP-NODE) (goal TSP-NODE))
  (point-distance (point node) (point goal)))

(defmethod $ ((node TSP-NODE))
  (if-let (parent (parent node))
	  (+ (distance node parent) ($ parent))
	  0))

(defmethod $ ((node epsilon))
  ($ (parent node)))

(defmethod $ ((node terminal))
  (+ ($ (parent node)) (distance (parent node) (first (ancestors node)))))

(defmethod make-children ((node TSP-NODE))
  (let* ((evaluated (mapcar #'node-name (stack node))))
    (if-let (children-names (set-difference *all-nodes-list* evaluated))
	    (mapcar #'(lambda (name) 
			(make-instance 'tsp-node 
				       :parent node
				       :point (get-loc name)
				       :name name)) children-names)
	    (list (create-terminal node)))))

(defun nearest-neighbor (tree)
  (labels ((a (node)
	     ($min (seq node))))
    (k-omega (selection)
	     (infinity 1 infinity 1)

	     (typep (root-node tree) 'terminal)

	     (select (root-node tree))

	     (setf selection (a (root-node tree)))
		    

	     (setf (root-node tree) selection))

    (mapcar #'node-name (ancestors (root-node tree)))))

(defun next-nearest-neighbor (tree)
  (labels ((a (node)
	     ($min (seq node))))
    (k-omega (selection)
	     (infinity 2 infinity 2)

	     (typep (root-node tree) 'terminal)

	     (select (root-node tree))

	     (setf selection (a (root-node tree)))
		    

	     (setf (root-node tree) selection))

    (mapcar #'node-name (ancestors (root-node tree)))))

(defun next-next-nearest-neighbor (tree)
  (labels ((a (node)
	     ($min (seq node))))
    (k-omega (selection)
	     (infinity 3 infinity 3)

	     (typep (root-node tree) 'terminal)

	     (select (root-node tree))

	     (setf selection (a (root-node tree)))
		    

	     (setf (root-node tree) selection))

    (mapcar #'node-name (ancestors (root-node tree)))))

(defun solved-in-limit (tree)
  (labels ((a (node)
	     ($min (seq node))))
    (k-omega (selection)
	     (infinity infinity infinity infinity)

	     (typep (root-node tree) 'terminal)

	     (select (root-node tree))

	     (setf selection (a (root-node tree)))
		    

	     (setf (root-node tree) selection))

    (mapcar #'node-name (ancestors (root-node tree)))))

(defun mem-solved-in-limit (tree)
    (labels ((a (node)
	       ($min (seq node))))
      (k-omega (selection)
	       (infinity infinity infinity infinity)

	       (typep (root-node tree) 'terminal)

	       (select (root-node tree))

	       (setf selection (a (root-node tree)))
		    

	       (setf (root-node tree) selection))

      (mapcar #'node-name (ancestors (root-node tree)))))


(location A 5 2)
(location B 6 7)
(location C 3 4)
(location D 4 6)
(location E 2 4)
(location F 6 1)
(location G 0 8)
(location H 4 3)
(location I 7 9)
(location J 10 2)

(defun tree (point) (make-instance '$tree :root-node 
				   (make-instance 'tsp-node
						  :name point
						  :point (get-loc point))))