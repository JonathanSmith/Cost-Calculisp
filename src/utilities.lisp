(in-package :utilities)

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
	  ,@(mapcar #'(lambda (s) `(utilities:memoize ',s)) symbols)
	  ,@body)
     (progn ,@(mapcar #'(lambda (s) `(utilities:unmemoize ',s)) symbols))))