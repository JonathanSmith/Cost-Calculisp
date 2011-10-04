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

(defclass A*-node ($node)
  ((name :accessor node-name :initarg :name)
   (marked? :accessor marked? :initform nil)
   (point :accessor point :initarg :point)
   (connections :accessor connections :initarg :connections)))

(defclass A*-goal (A*-node terminal)
  ())

(defparameter *all-nodes* (make-hash-table))

(defmacro location (name x y &rest connections)
  (let ((location (gensym "point")))
    `(let ((,location (make-instance 'A*-node
				       :name ',name
				       :point (make-point :x ,x :y ,y)
				       :connections ',connections)))
	 (setf (gethash ',name *all-nodes*) ,location))))

(defmacro goal (name x y &rest connections)
  (let ((goal (gensym "goal")))
    `(let ((,goal (make-instance 'A*-goal
				   :name ',name
				   :point (make-point :x ,x :y ,y)
				   :connections ',connections)))
	 (setf (gethash ',name *all-nodes*) ,goal)
	 (setf (gethash 'goal *all-nodes*) ,goal))))

(defun get-loc (name)
  (gethash name *all-nodes*))

(defmacro square (num)
  (let ((gnum (gensym "num")))
    `(let ((,gnum ,num))
       (* ,gnum ,gnum))))

(defun point-distance (np gp)
  (sqrt (+ (square (- (x np) (x gp))) (square (- (y np) (y gp))))))

(defmethod distance ((node A*-node) (goal A*-node))
  (point-distance (point node) (point goal)))

(defmethod $ ((node A*-node))
  (if-let (parent (parent node))
    (+ (distance node parent) ($ parent))
    0))

(defmethod $ ((node epsilon))
  (+ (distance (parent node) (get-loc 'goal)) ($ (parent node))))

(defmethod $ ((node A*-goal))
  -1)
  
(defmethod make-children ((node A*-node))
  (let ((children 
	 (set-difference ;;though shalt not introduce circularities into thine tree.
	  (mapcar #'get-loc (connections node))
	  (ancestors node))))
    (setf children (filter #'(lambda (x) (not (marked? x))) children))
    (mapcar #'(lambda (child) 
		(setf (marked? child) t)
		(setf (parent child) node)) children)
    (if (= (length children) 0)
	(error "length children error")
	children)))

(defmethod make-children ((node A*-goal))
  nil)

(defun A* (current)
  (labels ((a (node) 
	     (let ((s (seq node)))
	       ($min s))))
    (k-omega ((root current)
	      (selection root))
	     (infinity 1 infinity infinity)
	     (when (typep selection 'terminal)
	       (setf current selection))
	     (select selection)
	     (setf selection (a root))
	     nil)

    (mapcar #'node-name (stack current))))

(defun run-a* (node)
  (setf *all-nodes* (make-hash-table))
  (goal A 5 2 B C)
  (location B 6 7 C D A)
  (location C 3 4 E B A)
  (location D 4 6 B E)
  (location E 2 4 C D F)
  (location F 6 1 E G)
  (location G 0 8 F)
  (a* (get-loc node)))