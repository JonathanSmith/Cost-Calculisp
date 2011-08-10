(in-package :fibers)

(defvar max-priority 15)
(defvar min-priority 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct proc 
  (priority min-priority)
  (channel (channel))
  (mutex (sb-thread:make-mutex))
  (heap nil)
  (stack nil)
  (program nil)
  (ptr nil))

(defstruct fun
  (name)
  (program)
  (vars-len))

(defstruct scheduler ()
	   (queue nil)
	   (waitqueue (sb-thread:make-waitqueue))
	   (mutex (sb-thread:make-mutex)))

(defvar unbound '#:unbound)
(defvar empty '#:empty)
(defvar splice '#:splice)
(defvar yield)
(defvar priority)
(defvar mbox)
(defvar execution)
(defvar heap)
(defvar stack)
(defvar ptr )
(defvar name)

(defun create-scheduler ()
  (let ((a (list empty)))
    (nconc a a)
    (make-scheduler :queue a)))

(defvar scheduler );(create-scheduler))

(defvar completed nil)

(defvar bind-translation nil)




(defmacro with-proc (proc &body body)
  (let ((genv (gensym "env")))
    `(let* ((,genv ,proc))
       (when ,genv
	 (let* ((mbox (fibers::proc-channel ,genv))
		(priority (fibers::proc-priority ,genv))
		(heap (fibers::proc-heap ,genv))
		(stack (fibers::proc-stack ,genv))
		(execution (fibers::proc-program ,genv))
		(ptr (fibers::proc-ptr ,genv))
		(yield nil))

	   (unwind-protect 
		(progn ,@body)
	     (setf (fibers::proc-priority ,genv) priority)
	     (setf (fibers::proc-heap ,genv) heap)
	     (setf (fibers::proc-program ,genv) execution)
	     (setf (fibers::proc-stack ,genv) stack)
	     (setf (fibers::proc-ptr ,genv) ptr)))))))

(defun cpush (item cyclic)
  ;(format t "~s~%" (list item cyclic))
  (setf (cdr cyclic) (cons item (cdr cyclic))))

(defmacro cpop (cyclic)
  (let ((c1 (gensym))
	(c2 (gensym)))
    ` (progn
	(let ((,c1 ,cyclic)) 
	  (when (eql (cadr ,c1) empty)
	    (pop ,cyclic)))
	(let* ((,c2 ,cyclic)
	       (item (cadr ,c2)))
	  (if (eql (car ,c2) (caddr ,c2))
	      (setf ,cyclic (let ((li (list empty)))
			      (nconc li li)))
	      (setf (cdr ,cyclic) (cddr ,cyclic)))
	  item))))

(defun spush (item scheduler)
  (sb-thread:with-mutex ((scheduler-mutex scheduler))
    (cpush item (scheduler-queue scheduler))
    (pop (scheduler-queue scheduler))
    (sb-thread:condition-notify (scheduler-waitqueue scheduler))))

(defun spop (scheduler)
  ;(format t "~a~%" scheduler)  
  (sb-thread:with-recursive-lock ((scheduler-mutex scheduler))
    (let ((q (scheduler-queue scheduler)))
      (if (and (eql (car q) empty) (eql (cadr q) empty))
	  (progn
	    (sb-thread:condition-wait  
	     (scheduler-waitqueue scheduler)
	     (scheduler-mutex scheduler))
	    (spop scheduler))
	  (cpop (scheduler-queue  scheduler))))))


(defun schedule (proc)
  (spush proc scheduler))

(defun spawn (fn &rest args)
  (let ((proc (make-proc)))
    (with-proc proc (apply #'call (cons fn args)))
    (schedule proc)
    proc))

(defun yield ()
  (setf yield t))

(defun peek ()
  (first stack))

(defun next ()
  (incf (first ptr)))

(defmacro bind (var val)
  `(progn (setf ,var ,val) (next)))

(defun jump (i)
  (setf (first ptr) i))

(defmacro jump-when (condition i)
  `(if ,condition (fibers::jump ,i) (next)))


(defun call (function &rest args)
  (dolist (arg (nreverse args))
    (push arg stack))
  ;(format t "~a~%" stack)
  (push (make-array (fun-vars-len function)) heap)
  (push (fun-program function) execution)
  (when ptr (next))
  (push 0 ptr))

(defun exit-proc ()
  (pop heap)
  (pop execution)
  (pop ptr))

(defun *-> (channel val)
    (sb-thread:with-mutex ((channels::channel-mutex channel) :wait-p nil)
      (enqueue val (channels::channel-queue channel))
      (sb-thread:condition-notify (channels::channel-waitqueue channel))
      (next)))

(defun <-* (channel)
  (sb-thread:with-mutex ((channels::channel-mutex channel) :wait-p nil)
    (let ((qval (dequeue (channels::channel-queue channel))))
      (if (eq qval queue-empty)
	  (yield)
	  (progn
	    (push qval stack)
	    (next))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-var-subs (body)
    (let ((max-var 0)
	  (counter 0)
	  var-subs)
      (mapcar #'(lambda (x)
		  (cond ((and (listp x) (eql (first x) 'bind) (not (assoc (second x) var-subs)))
			 (push (list (second x) 
				     `(aref (first heap) ,counter)) var-subs)
			 (incf counter)
			 (when (> counter max-var)
			   (setf max-var counter)))
			((and (listp x) (eql (first x) 'unbind))
			 (decf counter))))
	      body)
      (values var-subs max-var)))

  (defun labelp (symbol)
    (and (symbolp symbol)
	 (equal (elt (symbol-name symbol) 0) #\@)))

  (defun get-tags (body)
    (let ((counter 0)
	  taglist)
      (values (mapcan #'(lambda (instruction)
			  (if (labelp instruction)
			      (progn (push (list instruction counter) taglist)
				     nil)
			      (progn (incf counter) (list instruction)))) body) taglist))))

 (defun unbindp (li)
   (and (listp li) (eql (first li) 'unbind)))

(defmacro progm (fnname taglist body)
  (multiple-value-bind (binding count) (get-var-subs body)
    (setf body (filter #'(lambda (x) (not (unbindp x))) body))
    `(defparameter ,fnname (fibers::make-fun 
			    :name ,(symbol-name fnname) 
			    :vars-len ,count
			    :program  
			    (symbol-macrolet ,taglist
			      (symbol-macrolet ,binding
				(vector 
				 ,@(mapcar #'(lambda (x)
					       `(lambda ()
						;(format t "~a: ~a~%" name ',x)
						  ,x))
					   body))))))))
(defmacro aprogm (taglist body)
  (multiple-value-bind (binding count) (get-var-subs body)
    (setf body (filter #'(lambda (x) (not (unbindp x))) body))
    `(make-fun 
      :name "ANON-FLAMBDA" 
      :vars-len ,count
      :program  
      (symbol-macrolet ,taglist
	(symbol-macrolet ,binding
	  (vector 
	   ,@(mapcar #'(lambda (x)
			 `(lambda ()
					;(format t "~a: ~a~%" name ',x)
			    ,x))
		     body)))))))


(defun priority-run ()
  (let ((proc (spop scheduler)))
    (with-proc proc
      (dotimes (ti priority)
	(if execution
	    (if yield 	(return)
		(funcall (aref (first execution) (first ptr))))
	    (return-from priority-run nil))))
    (spush proc scheduler)))

(defun priority-monitor-run ()
  (let ((proc (spop scheduler)))
    (with-proc proc
      (dotimes (ti priority)
	(if execution
	    (if yield 	(return)
		(funcall (aref (first execution) (first ptr))))
	    (return-from priority-monitor-run nil)))
      (if (and yield (> priority min-priority))
	  (incf priority)
	  (when (< priority max-priority)
	    (decf priority))))
    (spush proc scheduler)))

(defun run ()
  (sb-thread:make-thread
   #'(lambda () (do ()
		    (NIL)
		  (priority-run)))))

(defun monitor-run ()
  (sb-thread:make-thread
   #'(lambda () (do ()
		    (NIL)
		  (priority-monitor-run)))))

(defun syncthread (chan fn)
  (sb-thread:make-thread
   #'(lambda () (do ()
		    (NIL)
		  (apply fn (<-* chan))))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar macs (make-hash-table)))

(defmacro dmac (&body body)
  `(eval-when (:load-toplevel :compile-toplevel)
     (setf (gethash ',(first body) fibers::macs) (lambda ,@(rest body)))))

(dmac if (condition then else)
  (let ((then-tag (gensym "@then"))
	(end-tag (gensym "@end")))
    `(,splice
      (jump-when ,condition ,then-tag)
      ,else
      (jump ,end-tag)
      ,then-tag
      ,then
      ,end-tag)))

(defun remote (chan fn &rest args)
  (-> chan (lambda () (apply fn args))))

(dmac receive (&optional val)
      (if val
	  `(,splice
	    (<- mbox)
	    (bind ,val (pop stack)))
	  `(<- mbox)))

(dmac progn* (&rest body)
  (cons splice body))

(dmac let (bindings &rest body)
      `(,splice
	,@(mapcar #'(lambda (x) `(bind ,(first x) ,(second x))) bindings)
	,@body
	,@(mapcar #'(lambda (x) `(unbind ,(first x))) bindings)))

(dmac do (condition &rest body)
      (let ((start-tag (gensym "@start"))
	    (end-tag (gensym "@end")))
	`(,splice
	  ,start-tag
	  (jump-when ,condition ,end-tag)
	  (progn* ,@body)
	  (jump ,start-tag)
	  ,end-tag)))

(defun macro (symb) (gethash symb macs))

(defun expand (body)
  (let* ((counter 0)
	 (expanded (mapcar #'(lambda (x)
			       (if (and (listp x) (macro (first x)))
				   (apply (macro (first x)) (rest x))
				   x)) body))
	 (spliced (mapcan #'(lambda (x)
			      (cond 
				((and (listp x) (eql (first x) splice))
				 (progn
				   (incf counter)
				   (rest x)))
				(t (list x)))) expanded)))
    (if (> counter 0)
	(expand spliced)
	spliced)))

(defmacro define (name args &body body)
  (let* ((bindings (mapcar #'(lambda (x) `(bind ,x (pop stack))) args))
	 (expanded (expand `(,(cons splice bindings) ,(cons splice body) (exit-proc)))))
    (multiple-value-bind (body2 taglist) (get-tags expanded)
    `(fibers::progm ,name ,taglist ,body2))))

(defmacro flambda (args &body body)
  (let* ((bindings (mapcar #'(lambda (x) `(bind ,x (pop stack))) args))
	 (expanded (expand `(,(cons splice bindings) ,(cons splice body) (exit-proc)))))
    (multiple-value-bind (body2 taglist) (get-tags expanded)
      `(fibers::aprogm ,taglist ,body2))))

#|
(progn
  (define ping (to i io)
    (-> to mbox)
    (receive)
    (progn (pop stack) (next))
    (do (< i 2)
	(remote printer (lambda (x) (format t "Remote Ping: ~a~%" x)) i)
      (-> to (- i 1))
      (receive i)))

  (define pong (io)
    (let ((to nil)
	  (i nil))
      (receive to)
      (-> to :ok)
      (receive i)
      (do (< i 1)
	  (remote printer (lambda (x) (format t "Remote Pong: ~a~%" x)) i)
	(-> to (- i 1))
	(receive i))))



  (defun ping-pong (i)
    (let* ((a (spawn pong printer)))
      (spawn ping (proc-channel a) i printer))
    nil))
|#

#|(progn
    (define ping (to from i)
      (-> to i)
      (do (< i 2)
	  (<- from)
	(bind i (pop stack))
	(progn (format t "~a ~a~%" "PING" i) (next))
	(-> to (- i 1))))

    (define pong (to from)
      (<- from)
      (let ((i (pop stack)))
	(do (< i 1)
	    (progn (format t "~a ~a~%" "PONG" i) (next))
          ;(ping-pong i)
	  (-> to (- i 1))
	  (<- from)
	  (bind i (pop stack)))))

    (defun ping-pong (i)
      (let ((arec (channel))
	    (brec (channel)))
	 
	(spawn ping arec brec i)
	(spawn pong brec arec)
	nil)))|#
		  

#|
(defvar a)
(defvar b)
(defun ping-pong (i)
  (let ((arec (channel))
	(brec (channel)))
    (spawn ping arec brec i)
    (spawn pong brec arec)))


(progm ping
  (bind to (pop stack))
  (bind from (pop stack))
  (-> to (pop stack))
  (<- from)
  (bind v (pop stack))
  (progn (format t "ping: ~a~%" v) (next))
  (-> to (- v 1))
  (jump-when (> v 1) 3)
  (exit-proc))

(progm pong
  (bind to (pop stack))
  (bind from (pop stack))
  (<- from)
  (bind v (pop stack))
  (progn (format t "pong: ~a~%" v) (next))
  (-> to (- v 1))
  (jump-when (> v 1) 2)
  (exit-proc))

(defun tst ()
  (dotimes (i 1000000)
    (spawn factorial 4)))

(defun tst2 ()
  (dotimes (i 1000000)
    (spawn nothing nil)))

(progm factorial
  (jump-when (= (peek) 1) 2)
  (call factorial (- (peek) 1))
  (mul)
  (exit-proc))

(progm nothing
  (identity 1))

|#