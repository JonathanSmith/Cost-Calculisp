;(asdf::oos 'asdf::load-op :cost-calc)
(in-package "COST-CALC")

(defclass tic-tac-toe ($node) 
  ((pmoves :accessor pmoves :initform nil)
   (move :accessor move :initarg :move :initform nil)))

(defmethod print-object ((node tic-tac-toe) stream)

  (let ((moves (mapcar #'move (stack node)))
	(board (make-array 9 :initial-element nil)))
    (let ((count 1))
      (dolist (i (rest moves))
	(setf (aref board (- i 1)) (if (oddp count) #\x #\o))
	(incf count)))

    (labels ((tile (num)
	       (or (aref board num) (format nil "~a"(1+ num)))))

      
      (format stream "~&|~a|~a|~a|~%" (tile 0) (tile 1) (tile 2))
      (format stream "|~a|~a|~a|~%" (tile 3) (tile 4) (tile 5))
      (format stream "|~a|~a|~a|~%" (tile 6) (tile 7) (tile 8)))))

(defun minimax (tree)
  (labels ((a (node) ($min (mp #'b (seq node))))
	   (b (node) ($max (mp #'a (seq node)))))
    (k-omega (selection (root (root-node tree)))
	     (1 infinity infinity 1)
	     NIL
	     (select root)
	     (setf selection (a root))
	     (execute tree selection))
    tree))



(defun maximin (tree)
  (labels ((a (node) ($max (mp #'b (seq node))))
	   (b (node) ($min (mp #'a (seq node)))))
    (k-omega (selection (root (root-node tree)))
	     (1				;i
	      infinity			;k
	      infinity			;b
	      1				;n
	      )
	     NIL

	     (select root)
	    
	     (setf selection (a root))
	    
	     (execute tree selection)))
  tree)

(defvar win-conditions '((1 2 3) (4 5 6) (7 8 9)
			 (1 4 7) (2 5 8) (3 6 9)
			 (1 5 9) (3 5 7)))
(defvar possible-moves '(1 2 3 4 5 6 7 8 9))

 

(defun pminimax (tree)
  (let ((chan (channel)))
    (labels ((a (node) ($min (mp #'b (seq node))))
	     (b (node) ($max (mp #'a (seq node))))
	     (c (node) (-> chan ($max (mp #'a (seq node)))))
	     (d (node)
	       (let* ((s (seq node))
		      (nodefns (mapcar #'curry (repeat #'c) s)))
		 (apply #'par nodefns)
		 ($min (receive chan (length s))))))
 
      (k-omega (selection (root (root-node tree)))
	       
	       (1 infinity infinity 1)

	       NIL
	       (select root)

	       (setf selection (d root))

	       (execute tree selection))))
  tree)


(defun pmaximin (tree)
  (let ((chan (channel)))
    (labels ((a (node) ($max (mp #'b (seq node))))
	     (b (node) ($min (mp #'a (seq node))))
	     (c (node) (-> chan (b node)))
	     (d (node)
	       (let* ((s (seq node))
		      (nodefns (mapcar #'curry (repeat #'c) s)))
		 (apply #'par nodefns)
		 ($max (receive chan (length s))))))
 
      (k-omega (selection (root (root-node tree)))
	       
	       (1 infinity infinity 1)

	       NIL
	       (select root)
	       (setf selection (d root))

	       (execute tree selection))))
  tree)

#|(defun pmaximin (tree)
  (negation (pminimax tree)))
|#
(defmethod $ ((node tic-tac-toe))
  (let ((moves (mapcar #'move (stack node))))
    (calc-result moves)))

(defun calc-result (moves)
  (setf moves (rest moves))
  (let (xs os)
    (do* ((x (car moves) (car rmoves))
	  (rmoves (cdr moves) (cdr rmoves))
	  (o (car rmoves) (car rmoves)))
	 ((not (or x o)))
      (setf rmoves (cdr rmoves))
      (push x xs)
      (dolist (condition win-conditions)
	(when (not (set-difference condition xs))
	  (return-from calc-result -1)))
      (push o os)
      (dolist (condition win-conditions)
	(when (not (set-difference condition os))
	  (return-from calc-result 1))))
    0))

(defmethod make-children ((node tic-tac-toe))
  (let ((moves (mapcar #'move (stack node))))
    
    (let ((gmoves (set-difference possible-moves moves)))
      (if gmoves
	  (let (children)
	    (dolist (move gmoves)
	      (push (make-instance 'tic-tac-toe :move move :parent node) children))
	    (setf (%children node) children)
	    children)
	  (list (create-terminal node))))))

;;;;;;



;(defparameter tree (make-instance '$tree :root-node (make-instance 'tic-tac-toe)))
;(setf (sb-ext:bytes-consed-between-gcs) 201326592)
;(minimax tree)
;(pminimax tree)
;(sb-ext:gc :full t)

(defparameter *tree* (make-instance '$tree :root-node (make-instance 'tic-tac-toe)))

(defun play ()
  (let ((tree (make-instance '$tree :root-node (make-instance 'tic-tac-toe))))
    (labels ((w/l/t? ()
	       (let* ((root (root-node tree))
		      (state (cost root)))
		 (cond ((eql state -1)
			(format t "~a x wins!" root)
			(return-from play nil))
		       ((eql state 1)
			(format t "~a o wins!" root)
			(return-from play nil))
		       ((eql state 0)
			(unless (and root (not (typep (first (children root)) 'terminal)))
			  (format t "draw!")
			  (return-from play nil))))))
	     
	     (player-sel ()
	       (let* ((root (root-node tree))
		      (moves (mapcar #'move (children root))))
		   
		 (format t "~a~%Make A Move~%Available-moves: ~a~%"
			 root moves)
		 (sleep .1)
		 (let ((selection (read)))
		   (dolist (node (children root))
		     (when (eql (move node) selection)
		       (return-from player-sel node)))
		   (player-sel))))
	     (computer-sel ()
	       (pmaximin tree))
	     (one-turn ()
	       (setf (root-node tree) (player-sel))
	       (w/l/t?)
	       (computer-sel)
	       (w/l/t?)))
      (do ()
	  (NIL)
	(one-turn)))))

