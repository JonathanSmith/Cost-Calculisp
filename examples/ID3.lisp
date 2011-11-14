(in-package "COST-CALC")

(defclass instance () ())

(defgeneric dataset (thing))
(defgeneric attribute-values (symbol dataset))
(defgeneric all-attributes (thing))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro def-dataset (name &body attributes)
    (let ((attribute-hash-sym (gensym (format nil "~a-attributes-hash" name)))
	  (attribute-instance-list-sym (gensym (format nil "~a-attributes-instance-list" name)))
	  (attribute-list-sym (gensym (format nil "~a-attribute-list" name)))
	  (classification-sym (gensym (format nil "~a-classifier" name)))
	  (class-symbol (intern (string-upcase(symbol-name name)) *package*))
	  (list-sym (gensym "list-sym")))

      `(let ((,attribute-hash-sym (make-hash-table))
	     (,attribute-instance-list-sym nil)
	     (,attribute-list-sym nil)
	     (,classification-sym nil)
	     )

	 ;;don't really want to modify these anyway.
	 (defmethod dataset ((,(gensym) (eql ',class-symbol))) ,attribute-instance-list-sym)
	 (defmethod attribute-values ((symbol symbol) (,(gensym) (eql ',class-symbol))) 
	   (gethash symbol ,attribute-hash-sym))
	 (defmethod all-attributes ((,(gensym) (eql ',class-symbol))) ,attribute-list-sym)
	 (defmethod classifier ((,(gensym) (eql ',class-symbol))) ,classification-sym)
       
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

	 (setf ,attribute-list-sym ',(mapcar #'first (butlast attributes)))
	 (setf ,classification-sym ',(first (first (last attributes))))
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
	     `(,constructor ,@(mapcar #'(lambda (sym) (list 'quote sym)) ,list-sym))))))))



(defclass id3-node ($node)
  ((dataset-symbol :accessor dataset-symbol :initarg :dataset-symbol)))

(defclass id3-attribute (id3-node)
  ((attribute :accessor attribute :initarg :attribute)))

(defclass id3-value (id3-node)
  ((value :accessor value :initarg :value)
   (%subset :accessor %subset :initarg :subset)))

#|(defclass id3-terminal (id3-node terminal)
  (value :accessor value :initarg :value))|#

(defclass id3-root (id3-node) ())

(defmethod find-subset ((node id3-value))
  (let ((parent-subset (subset (parent node)))
	(parent-attribute (attribute (parent node)))
	(node-value (value node)))
    (filter (lambda (instance) (eql (funcall parent-attribute instance) node-value)) parent-subset)))

(defmethod subset ((node id3-root))
  (dataset (dataset-symbol node)))

(defmethod subset ((node id3-attribute))
  (subset (parent node)))

(defmethod subset ((node id3-value))
  (if (%subset node)
      (%subset node)
      (let ((subset (find-subset node)))
	(setf (%subset node) subset)
	subset)))

(defun node-attribute-values (node)
  (attribute-values (attribute node) (dataset-symbol node)))
 
(defmethod all-attributes ((node id3-node))
  (all-attributes (dataset-symbol node)))

(defmethod used-attributes ((node id3-node))
  (mapcar #'attribute (remove-if-not (lambda (object) (typep object 'id3-attribute)) (ancestors node))))

(defmethod remaining-attributes ((node id3-node))
  (set-difference (all-attributes node) (used-attributes node)))

(defmethod make-children ((node id3-root))
  (let ((attributes (all-attributes node)) 
	children)
    (dolist (attr attributes)
      (push (make-instance 'id3-attribute
			   :parent node
			   :dataset-symbol
			   (dataset-symbol node)
			   :attribute attr) children))
    children))

(defmethod make-children ((node id3-value))
  (let ((attributes (remaining-attributes node))
	children)
    (dolist (attr attributes)
      (push (make-instance 'id3-attribute
			   :parent node
			   :dataset-symbol
			   (dataset-symbol node)
			   :attribute attr) children))
    children))

(defmethod make-children ((node id3-attribute))
  (let ((values (node-attribute-values node))
	children)
    (dolist (value values)
      (push (make-instance 'id3-value
			   :parent node
			   :subset nil
			   :dataset-symbol
			   (dataset-symbol node)
			   :value value) children))
    children))

(defun entropy (node &optional (dataset (subset node)))
  ;;this is a basic entropy function
  (let* ((attribute (attribute node))
	 (classifier (classifier (dataset-symbol node)))
	 (values (node-attribute-values node))
	 (proportion-list nil)
	 (set-size (length dataset)))
    (format t "~s~%" (mapcar (lambda (instance) (list (funcall attribute instance)
						      (funcall classifier instance))) dataset)) 

    (dolist (value values)
      (let ((count 0))
	(mapcar #'(lambda (instance) 
		    (when (eql (funcall attribute instance) value)
		      (incf count)))
		dataset)
	(push (if (= set-size 0) 0 (/ count set-size)) proportion-list)))
    (- (reduce #'+ (mapcar #'(lambda (c)
			       (if (> c 0) (* (- c) (log c 2)) 0)) proportion-list)))))

(defmethod gain ((node id3-attribute) (next-node id3-attribute))
  (%gain node next-node))

(defmethod gain ((node id3-value) (next-node id3-attribute))
  (gain (parent node) next-node))

(defmethod gain ((node id3-root) (next-node id3-attribute))
  ;;this needs to do something else. i dont know what!
  (entropy next-node))

(defun %gain (node next-node)
  (let* ((dataset (subset node))
	 (total (length dataset))
	 (values (node-attribute-values next-node))
	 (next-attribute (attribute next-node))
	 (partition nil)
	 (countlist nil))

    (when (= total 0)
      (return-from %gain 0))

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
  (format t "e~%")
  (entropy node))

(defmethod $ ((node id3-attribute))
  (format t "f~%")
  (gain (parent node) node))

(defmethod id3 ((node id3-root))
  (let ((k 2) (b infinity) (n 1))
    (format t "a~%")
    (select node)
    (format t "b~%")
    (let ((first-attribute ($max (children node))))
      (list (attribute first-attribute) (mapcar #'id3 (children first-attribute))))))


(defmethod id3 ((node id3-attribute))
  (let ((k 1) (b infinity) (n 1))
    (format t "c~%")
    (select node)
    (format t "d~%")
    (mapcar #'id3 (children node))))

(defmethod id3 ((node id3-value))
  (let ((k 2) (b infinity) (n 1))
    (format t "a~%")
    (select node)
    (format t "b~%")
    (let ((max-attribute ($max (children node))))
      (if (typep max-attribute 'id3-attribute)
	  (list (value node) (attribute max-attribute) (mapcar #'id3 (children max-attribute)))
	  (list (value node) (most-common-result node #'outcome))))))

(defun most-common-result (node function)
  (mapcar function (subset node)))
  


#|
       a
      / \
     b   b
    / \  
   a   a
|#

(def-dataset house 
	     (district suburban rural urban)
	     (house-type detached semi-detached terrace)
	     (income high low)
	     (previous yes no)
	     (outcome nothing responded))
(progn
  (house Suburban Detached High No Nothing)
  (house Suburban Detached High Yes Nothing)
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

(defun run-id3-house () (id3 (make-instance 'id3-root :dataset-symbol 'house)))

#|
(def-dataset customer-data
  (age "0-12" "13-21")
  (education "High School" "College")
  (income "10000")
  (marital-status "YES" "NO")
  (purchase? "YES" "NO"))
|#

#|(def-dataset
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