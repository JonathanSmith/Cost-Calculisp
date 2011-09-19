(in-package "COST-CALC")

(defclass instance () ())

(defgeneric dataset (thing))
(defgeneric attribute-values (symbol dataset))
(defgeneric all-attributes (thing))

(defmacro def-dataset (name &body attributes)
  (let ((attribute-hash-sym (gensym (format nil "~a-attributes-hash" name)))
	(attribute-instance-list-sym (gensym (format nil "~a-attributes-instance-list" name)))
	(attribute-list-sym (gensym (format nil "~a-attribute-list" name)))
	(class-symbol (intern (string-upcase(symbol-name name)) *package*))
	(list-sym (gensym "list-sym")))

    `(let ((,attribute-hash-sym (make-hash-table))
	   (,attribute-instance-list-sym nil)
	   (,attribute-list-sym nil))

       ;;don't really want to modify these anyway.
       (defmethod dataset ((,(gensym) (eql ',class-symbol))) ,attribute-instance-list-sym)
       (defmethod attribute-values ((symbol symbol) (,(gensym) (eql ',class-symbol))) 
	 (gethash symbol ,attribute-hash-sym))
       (defmethod all-attributes ((,(gensym) (eql ',class-symbol))) ,attribute-list-sym)
       
       (defclass ,class-symbol (instance)
	 (,@(mapcar 
	     #'(lambda (attr) 
		 (let ((attr-name (first attr)))
		   `(,attr-name
		     :accessor ,attr-name 
		     :initarg 
		     ,(intern (string-upcase (symbol-name attr-name)) "KEYWORD")
		     :initform nil)))
	     attributes)))

       (setf ,attribute-list-sym ',(mapcar #'first attributes))
       ,@(mapcar 
	  #'(lambda (attr)
	      (let* ((name (first attr))
		     (cases (rest attr)))
		`(setf (gethash ',name ,attribute-hash-sym) ',cases)))
	  attributes)

       (defun ,(intern (string-upcase (format nil "make-~a" (symbol-name name))) *package*) ,(mapcar #'first attributes)
	 (push (make-instance ',name ,@(mapcan #'(lambda (attr) (list (intern (string-upcase (symbol-name attr)) "KEYWORD") attr))
					       (mapcar #'first attributes))) ,attribute-instance-list-sym))
       (defmacro ,name (&rest ,list-sym)
	 (let ((constructor ',(intern (string-upcase (format nil "make-~a" (symbol-name name))) *package*)))
	   `(,constructor ,@(mapcar #'(lambda (sym) (list 'quote sym)) ,list-sym))))
       )))


(defclass id3-node ($node)
  ((attribute :accessor attribute :initarg :attribute)
   (value :accessor value :initarg :value)
   (dataset-symbol :accessor dataset-symbol :initarg :dataset-symbol)
   (subset :accessor subset :initarg :subset)))

(defclass id3-root (id3-node) ())

(defmethod dataset ((node id3-node))
  (subset node))

(defmethod dataset ((node id3-root))
  (dataset (dataset-symbol node)))

(defun node-attribute-values (node)
  (attribute-values (attribute node) (dataset-symbol node)))
 
(defmethod all-attributes ((node id3-node))
  (all-attributes (dataset-symbol node)))

(defmethod make-children ((node id3-node))
  (let* ((dataset (dataset node))
	 (available-attributes  
	  (if (parent node)
	      (set-difference (all-attributes node) 
			      (cons (attribute node) (mapcar #'attribute (ancestors node))))
	      (set-difference (all-attributes node) (list (attribute node))))))
    

    (if available-attributes
	(let (children)
	  (dolist (attr available-attributes)
	    (let* ((values (attribute-values attr (dataset-symbol node)))
		   val-nodes)
	      (dolist (val values)
		(let ((subset (filter (lambda (instance)
					(eql (funcall attr instance) val))
				      dataset)))
		  (push (make-instance 'id3-node
				       :attribute attr
				       :parent node
				       :dataset-symbol (dataset-symbol node)
				       :value val
				       :subset subset) val-nodes)))
	      (push val-nodes children)))
	 ; (format t "~s~%" children)
	  (setf (%children node) (apply #'concatenate 'list children)))
	(list (create-terminal node)))))

(defun entropy (node &optional (dataset (dataset node)))
;;this is a basic entropy function
  (let* ((attribute (attribute node))
	 (values (node-attribute-values node))
	 (proportion-list nil)
	 (set-size (length dataset)))
    (dolist (value values)
      (let ((count 0))
	(mapcar #'(lambda (instance) 
		    (when (eql (funcall attribute instance) value)
		      (incf count)))
		dataset)
	(push (if (= set-size 0) 0 (/ count set-size)) proportion-list)))
    (reduce #'+ (mapcar #'(lambda (c) (if (> c 0) (* (- c) (log c 2)) 0)) proportion-list))))

(defun gain (node next-node)
  (let* ((dataset (dataset node))
	 (total (length dataset))
	 (values (node-attribute-values next-node))
	 (next-attribute (attribute next-node))
	 (partition nil)
	 (countlist nil))
   (when (= total 0)
     (return-from gain 0))
    (dolist (value values)
      (let ((sublist nil)
	    (count 0))
	(mapcar #'(lambda (instance) 
		    (when (eql (funcall next-attribute instance) value)
		      (incf count)
		      (push instance sublist))) dataset)
	(push count countlist)
	(push sublist partition)))

    (- (entropy node) 
       (reduce #'+ (mapcar (lambda (subset count)
			     (/ (* count (entropy node subset)) total))
			   partition countlist)))))

(defmethod $ ((node id3-root))
  (entropy node))

(defmethod $ ((node id3-node))
  (gain (parent node) node))

(defmethod $ ((list list))
  (apply #'max (mapcar #'$ list)))

(defun id3 (root)
  (k-omega ((leaf ($max (children root))))
	   (infinity 1 infinity 1)
	      
	   (eql (class-of leaf) (find-class 'terminal))

	   (progn
	     (select leaf)
	     (format t "A~s~%" leaf))

	   (progn
	     (setf leaf  ($max (seq leaf)))
	     (format t "B~s~%" leaf))

	   (setf root leaf))
  root)


(def-dataset house
  (district suburban rural urban)
  (house-type detached semi-detached terrace)
  (income high low)
  (previous yes no)
  (outcome nothing responded))
(progn
  (house Suburban Detached High No Nothing)
  (house Suburban 	Detached 	High 	Yes 	Nothing)
  (house Rural 	Detached 	High 	No 	Responded)
  (house Urban 	Semi-detached 	High 	No 	Responded)
  (house Urban 	Semi-detached 	Low 	No 	Responded)
  (house Urban 	Semi-detached 	Low 	Yes 	Nothing)
  (house Rural 	Semi-detached 	Low 	Yes 	Responded)
  (house Suburban 	Terrace 	High 	No 	Nothing)
  (house Suburban 	Semi-detached 	Low 	No 	Responded)
  (house Urban 	Terrace 	Low 	No 	Responded)
  (house Suburban 	Terrace 	Low 	Yes 	Responded)
  (house Rural 	Terrace 	High 	Yes 	Responded)
  (house Rural 	Detached 	Low 	No 	Responded)
  (house Urban 	Terrace 	High 	Yes 	Nothing))

(defun run-id3-house () (id3 (make-instance 'id3-root
					    :dataset-symbol 'house
					    :value nil
					    :attribute nil
					    :subset nil
					    )))

#|
(def-dataset customer-data
  (age "0-12" "13-21")
  (education "High School" "College")
  (income "10000")
  (marital-status "YES" "NO")
  (purchase? "YES" "NO"))
|#

#|(def-dataset s
  (
D1	Sunny	Hot	High	Weak	No
D2	Sunny	Hot	High	Strong	No
D3	Overcast	Hot	High	Weak	Yes
D4	Rain	Mild	High	Weak	Yes
D5	Rain	Cool	Normal	Weak	Yes
D6	Rain	Cool	Normal	Strong	No
D7	Overcast	Cool	Normal	Strong	Yes
D8	Sunny	Mild	High	Weak	No
D9	Sunny	Cool	Normal	Weak	Yes
D10	Rain	Mild	Normal	Weak	Yes
D11	Sunny	Mild	Normal	Strong	Yes
D12	Overcast	Mild	High	Strong	Yes
D13	Overcast	Hot	Normal	Weak	Yes
D14	Rain	Mild	High	Strong	No|#