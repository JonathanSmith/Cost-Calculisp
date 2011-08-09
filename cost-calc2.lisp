;par with lookahead
;$of par does it have cost?
;what does it mean?
;

(in-package "COST-CALC")
(defvar infinity most-positive-fixnum)
(defvar k infinity)
(defvar b infinity)
(defvar n 1)
(defvar *threads* 8)

(defmacro if-let (binding then else)
  `(let (,binding)
     (if ,(first binding)
	 ,then
	 ,else
	 )))

(defun repeat (item)
  (let ((li (list item)))
    (nconc li li)))

(defun cycle (items)
  (let ((list (copy-list items)))
    (nconc list list)))

(defun split (lst by)
  (let ((list (copy-list lst)))
    (let (aux
	  (partition-number (truncate (length list) by)))
      (dotimes (i (- by 1))
	(let (l)
	  (dotimes (j partition-number)
	    (push (pop list) l))
	  (push (nreverse l) aux)))
      (push list aux)
      (nreverse aux))))

(defun mp (fn list)
  (mapcar #'(lambda (li) 
	      (if (or (typep li 'terminal)
		      (typep li 'epsilon))
		  li
		  (funcall fn li))) list))

(defun filter (fn list &aux aux)
  (map nil #'(lambda (x) (when (funcall fn x)
			   (push x aux))) list)
  (nreverse aux))

(defun curry (fn &rest items)
  #'(lambda (&rest rest) (apply fn (append items rest))))

(defun memoize (fn)
  (let ((nothing (gensym "nothing"))
	(hash (make-hash-table :test 'equalp)))
    (flet ((memoize (fun)
	     #'(lambda (&rest args)
		 (let ((answer (gethash args hash nothing)))
		   (if (eql answer nothing)
		       (let ((memo (apply fun args)))
			 ;(format t "~a, ~a~%" fun args)
			 (setf (gethash args hash) memo)
			 memo)
		       answer)))))
	     
      (cond ((functionp fn)
	     (memoize fn))
	    ((symbolp fn)
	     (if (fboundp fn)
		 (if (get fn :def)
		     (error "symbol ~a already memoized, unmemoize first~%" fn)
		     (let ((rfn (symbol-function fn)))
		       (setf (symbol-function fn) (memoize rfn))
		       (setf (get fn :def) rfn)))
		 (error "attempted memoize ~a isn't fbound~%" fn)))))))

(defun unmemoize (fn)
  (let ((def (get fn :def)))
    (when def (setf (symbol-function fn) def))
    (setf (get fn :def) nil)))

(defmacro with-memoizations (symbols &body body)
  `(unwind-protect 
	(progn   
	  ,@(mapcar #'(lambda (s) `(memoize ',s)) symbols)
	  ,@body)
     (progn ,@(mapcar #'(lambda (s) `(unmemoize ',s)) symbols))))
  

(defclass $tree ()
  ((root-node :accessor root-node :initarg :root-node :initform nil)))

(defclass $node ()
  ((parent :accessor parent :initarg :parent :initform nil)
   (%children :accessor %children :initarg :children :initform nil)))

(defclass epsilon ($node) ())
(defclass terminal ($node) ())

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
      (if-let (child (gen-next-child node))
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
  ;(format t "~a, ~a~%" node k)
  (cond ((%children node)
	; (format t "1~%")
	 (%children node))
	((> depth 0)
					;(format t "2~%")
	 (let ((children (make-children node)))
	   (setf (%children node) children)
	   children))
	((= depth 0)
	 ;(format t "3~%")
	 (unless (or (typep node 'epsilon)
		     (typep node 'terminal))
	   (let ((epsilon (list (create-epsilon node))))
	     ;(setf (%children node) epsilon)
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
		 ;(format t "s:~a~%" k)
		 (unless (or (typep nde 'terminal)
			     (typep nde 'epsilon))
		   (let ((children (children nde)))
		     (let ((k (- k 1)))
		       (mapcar #'select-1 children))))))
	(mapcar #'select-1 children))))
  nil)

(define select-1 (nde k)
  (if (not (or (typep nde 'terminal)
	       (typep nod 'epsilon)))
      (let ((children (children nde k)))
	(mapcar (lambda (node) (spawn select-1 node (- k 1))) children))))

(define select (node k)
  (if (not (typep node 'terminal))
      (let ((children (cond ((typep node 'epsilon)
			     (make-children node))
			    ((null node)
			     (error "wtf?"))
			    (t (children node)))))
	(mapcar (lambda (nde) (spawn select-1 nde k)) children))))
			    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defstruct q-node
    before
    after
    value)

  (defstruct queue
    (head)
    (tail))

  (defvar queue-empty '#:|queue-empty|)

;;i think you can do this with a circular list and a marker, rather than structs.. not sure.
;;this works fine for now, however.

  (defun enqueue (item queue)
    (let ((node (make-q-node :value item)))
      (cond ((and (eq (queue-head queue) nil) (eq (queue-tail queue) nil))
	     (setf (queue-head queue) node)
	     (setf (queue-tail queue) node))
	    (t
	     (let ((tail (queue-tail queue)))
	       (setf (q-node-after tail) node)
	       (setf (q-node-before node) tail)
	       (setf (queue-tail queue) node))))))

	 

  (defun dequeue (queue)
    (let ((head (queue-head queue)))
      (if head
	  (progn
	    (setf (queue-head queue) (q-node-after head))
	    (if (q-node-after head)
		(setf (q-node-before (q-node-after head)) nil)
		(setf (queue-tail queue) nil))
	    (q-node-value head))
	  queue-empty)))

  (defstruct channel 
    name
    mutex
    queue
    waitqueue)

  (defun channel (&optional namestring)
    (make-channel :name (or namestring "")
		  :waitqueue (sb-thread:make-waitqueue)
		  :mutex (sb-thread:make-mutex :name namestring)
		  :queue (make-queue)))

  (defmacro defchannel (symbol)
    `(defparameter ,symbol (channel ,(symbol-name symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun -> (channel val)
    (sb-thread:with-mutex ((channel-mutex channel))
      (enqueue val (channel-queue channel))
      (sb-thread:condition-notify (channel-waitqueue channel))))

  (defun <- (channel)
    (sb-thread:with-mutex ((channel-mutex channel))
      (let ((qval (dequeue (channel-queue channel))))
	(if (eq qval queue-empty)
	    (progn (sb-thread:condition-wait
		    (channel-waitqueue channel)
		    (channel-mutex channel))
		   (dequeue (channel-queue channel)))
	    qval))))

(defun receive (chan num &aux li)
  (dotimes (i num)
    (push (<- chan) li))
  li)
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
(defvar *thread-channels*)

(eval-when (:load-toplevel)
  (let (channels threads)
    (dotimes (i *threads*)
      (push (channel) channels))
    (setf threads 
	  (mapcar #'(lambda (channel)
		      (sb-thread:make-thread 
		       #'(lambda () 
			   (do ((fn (<- channel) (<- channel)))
			       (NIL)
			     (funcall fn)))))
		  channels))
    (setf *thread-channels* channels)))

(defun par (fns)
  (let ((chans (cycle *thread-channels*)))
    (dolist (fn fns)
      (-> (car chans) fn)
      (setf chans (cdr chans)))))

#|(defun par (fnargs)
  (let ((lambdas (mapcar #'(lambda (fnarg)
			     #'(lambda () 
				 (let ((k k)
				       (b b))
				 (apply (first fnarg) (rest fnarg)))))
			 fnargs)))
    (mapcar #'sb-thread:make-thread lambdas)))|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric execute (tree node))

(defmethod execute ((tree $tree) (node $node))
  (let ((ancestors (ancestors node)))
    (dotimes (i n)
      (let ((children (children (root-node tree))))
	(setf (root-node tree) (first (intersection ancestors children))))
      ;(format t "r: ~a~%" (root-node tree))
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