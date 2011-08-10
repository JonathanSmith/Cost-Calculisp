;par with lookahead
;$of par does it have cost?
;what does it mean?

(load "channels")
(load "utilities")
(load "fibers")

(in-package "COST-CALC")
(defvar infinity most-positive-fixnum)
(defvar k infinity)
(defvar b infinity)
(defvar n 1)
(defvar *threads* 8)

(defclass $tree ()
  ((root-node :accessor root-node :initarg :root-node :initform nil)))

(defclass $node ()
  ((parent :accessor parent :initarg :parent :initform nil)
   (%children :accessor %children :initarg :children :initform nil)))

(defclass epsilon ($node) ())
(defclass terminal ($node) ())

(defun mp (fn list)
  (mapcar #'(lambda (li) 
	      (if (or (typep li 'terminal)
		      (typep li 'epsilon))
		  li
		  (funcall fn li))) list))

(defun create-epsilon (parent)
  (make-instance 'epsilon :parent parent))

(defun create-terminal (parent)
  (make-instance 'terminal :parent parent))

(defgeneric gen-next-child (node))

(defgeneric make-children (node))
(defgeneric ancestors (node))

(defmethod make-children ((node $node))
  (let (ret)
    (dotimes (i b)
      (utilities::if-let (child (gen-next-child node))
	(push (gen-next-child node) ret)
	(return nil)))
    (nreverse ret)))

(defmethod make-children ((node epsilon))
  (let ((parent-node (parent node)))
    (let ((children (make-children parent-node)))
      (setf (%children parent-node) children))))

(defmethod make-children ((node terminal)) nil)

(defgeneric children (node &optional depth))

(defmethod children ((node $node) &optional (depth k))
  (cond ((%children node)
	 (%children node))
	((> depth 0)
	 (let ((children (make-children node)))
	   (setf (%children node) children)
	   children))
	((= depth 0)
	 (unless (or (typep node 'epsilon)
		     (typep node 'terminal))
	   (let ((epsilon (list (create-epsilon node))))
	     epsilon)))
	((< depth 0)
	 (error "Child generation to depth greater than maximum"))))

(defmethod ancestors ((node $node))
  (let (ancestors)
    (do ((parent (parent node) (parent parent)))
	((not parent))
      (push parent ancestors))
    ancestors))

(defun stack (node)
  (let ((stack (list node)))
    (do ((parent (parent node) (parent parent)))
	((not parent))
      (push parent stack))
    stack))

(defun select (node)
  (unless (typep node 'terminal)
    (let ((children (cond ((typep node 'epsilon)
			   (make-children node))
			  ((null node)
			   (error "wtf?"))
			  (t
			   (children node)))))
      (labels ((select-1 (nde)
		 (unless (or (typep nde 'terminal)
			     (typep nde 'epsilon))
		   (let ((children (children nde)))
		     (let ((k (- k 1)))
		       (mapcar #'select-1 children))))))
	(mapcar #'select-1 children))))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro choice (&rest choices)
    (let ((counter 0)
	  (passed (gensym "passed"))
	  guards
	  bodies)
;;; there should be a more efficient way to implement this,
;;; perhaps using an array?
;;; this should be fine for now, however.
      (map nil #'(lambda (choice) 
		   (let ((guard `(when ,(first choice)
				   (push ,counter ,passed)))
			 (body `((,counter) ,(second choice))))
		     (push guard guards)
		     (push body bodies)
		     (incf counter)
		     guard))  choices)

      (setf guards (nreverse guards))
      (setf bodies (nreverse bodies))
      `(let (,passed)
	 ,@guards
	 (case (nth (random (length ,passed)) ,passed)
	   ,@bodies))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;hmm how to 'build to depth 3'
;; and then evaluate 'max' of that.
;; this stuff will take a 'depth' limit
;; but doesn't really do quite what i want.

(defgeneric $ (item))

(defmethod $ ((node terminal))
  ($ (parent node)))

(defmethod $ ((node epsilon))
  ($ (parent node)))


(defun cost (item)
  ;(format t "cost: ~a~%" item)
  (cond 
    ((listp item)
     (if (null item)
	 0
	 (reduce #'+ (mapcar #'cost item))))
    (t ($ item))))

(defun $min (list)
  ;(format t "~a~%" list)
  (if (listp list)
      (let* ((min (list (first list)))
	     (min$ (cost min)))
	(dolist (next (rest list))
	  (let ((next$ (cost next)))
	    (cond ((< next$ min$)
		   (setf min$ next$)
		   (setf min (list next)))
		  ((= next$ min$)
		   (push next min)))))
	(nth (random (length min)) min))
      list))

(defun $max (list)
  (if (listp list)
      (let* ((max (list (first list)))
	     (max$ (cost max)))
	(dolist (next (rest list))
	  (let ((next$ (cost next)))
	    (cond ((> next$ max$)
		   (setf max$ next$)
		   (setf max (list next)))
		  ((= next$ max$)
		   (push next max)))))
	(nth (random (length max)) max))
      list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|(defun seq (fn nodes)
  (let ((k (- k 1)))
	 (mapcar #'(lambda (node)
	      (let ((children (children node)))
		(if (or (typep (first children) 'terminal)
			(typep (first children) 'epsilon))
		    (first children)
		      (funcall fn children)))) nodes)))|#
#|
(defun seq (node)
  (mapcar #'(lambda (node)
	      (let ((children (%children node)))
		(format t "~a ~a~%"  node children)
		(if (or (typep (first children) 'terminal)
			(typep (first children) 'epsilon))
		    (first children)
		    (funcall fn children)))) nodes))
|#

(defun seq (node &aux ret)
  ;(format t "~a~%" node)
  (labels ((seq-1 (nde)
	     (let ((children (%children nde)))
	      ; (format t "~a, ~a, ~a~%" n nde (%children nde))
	       (if (or 
		    (null children)
		    (typep nde 'terminal)
		    (typep nde 'epsilon)
		    (= n 0))
		   (push nde ret)
		   (let ((n (- n 1)))
		     (mapcar #'seq-1 children))))))
    (seq-1 node))
  ;(format t "~s~%" ret)
  ret)
	      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;this will be better with thread pooling when i get around to modifying thread pool library.
;;right now has possibility to hang if I create (for example) 51 threads in the same thread group...
;;could use make-thread to avoid this (easy fix) or hack on thread pool library to allow for
;;thread pools which will dynamically expand rather than queuing.

(defstruct thread-group
  (lock (sb-thread:make-mutex))
  (token-list nil))

(defvar *thread-pool* (let ((tp (thread-pool:make-thread-pool 50)))
			(thread-pool:start-pool tp)
			tp))

(defun add-callback (function thread-group thread-token callback)
  (lambda () 
    (funcall function)
      (sb-thread:with-mutex ((thread-group-lock thread-group))
	(setf (thread-group-token-list thread-group)
	      (remove thread-token (thread-group-token-list thread-group)))
	(unless (thread-group-token-list thread-group)
	  (funcall callback)))))
    
;;caveat here is that anything which happens 
;;consequent to a par must be within the 'continuation' body.

(defmacro par (functions &body continuation)
  (let ((continuation `(lambda () ,@continuation))
	(gcontsym (gensym))
	(thread-group (gensym)))
    `(let ((,thread-group (make-thread-group))) 
       (sb-thread:with-mutex ((thread-group-lock ,thread-group))
	 ,@(mapcar 
	    (lambda (function-code)
	      `(let ((,gcontsym (gensym)))
		 (push ,gcontsym (thread-group-token-list ,thread-group))
		 (thread-pool:add-to-pool cost-calc::*thread-pool* (cost-calc::add-callback ,function-code ,thread-group ,gcontsym ,continuation))))
	    functions)))))

#|(let ((out *standard-output*))
  (defun mklzfb (n)
    (labels ((fib (n) (if (> n 2)
			  (+ (fib (- n 1)) (fib (- n 2)))
			  1)))
      (lambda ()
	(let ((m n))
	  (format out "~%fib ~a = ~a~%" m (fib m)))))))
  (defun mklzfb2 (n)
    (lambda () (cost-calc::par ((mklzfb n) (mklzfb n)) (format t "subgroup done~%"))))|#

#|(cost-calc::par ((mklzfb2 35) (mklzfb2 30) (mklzfb 25) (mklzfb 31) (mklzfb 12) (mklzfb 10) ) (format t "all done~%") (force-output))|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric execute (tree node))

(defmethod execute ((tree $tree) (node $node))
  (let ((ancestors (ancestors node)))
    (dotimes (i n)
      (let ((children (children (root-node tree))))
	(setf (root-node tree) (first (intersection ancestors children))))
      )))

(defmacro k-omega (inits (max-t k b n) goal-body select-body exam-body exec-body)
  (let ((ti (gensym "ti"))
	(mt (gensym "mt"))
	(goal (gensym "goal"))
	(select (gensym "select")) 
	(exam (gensym "exam"))
	(exec (gensym "exec")))
    `(let* ,inits
       (let* ((,mt ,max-t)
	      (k ,k)
	      (b ,b)
	      (n ,n))
	 (labels ((,goal () ,goal-body)
		  (,select () ,select-body)
		  (,exam () ,exam-body)
		  (,exec () ,exec-body))
	   (do ((,ti 0 (+ ,ti 1)))
	       ((or (= ,ti ,mt) (,goal)))
	     (,select)
	     (,exam)
	     (,exec)))))))