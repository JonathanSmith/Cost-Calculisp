(in-package :channels)  

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
    `(defparameter ,symbol (channels:channel ,(symbol-name symbol))))

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