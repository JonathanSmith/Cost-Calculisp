(ql:quickload "thread-pool")

(defpackage utilities
  (:use :common-lisp)
  (:export #:if-let 
	   #:repeat
	   #:cycle
	   #:split
	   #:filter
	   #:curry
	   #:memoize
	   #:unmemoize
	   #:with-memoizations))

(defpackage channels
  (:use :common-lisp)
  (:export
   #:q-node
   #:queue
   #:queue-empty
   #:enqueue
   #:dequeue
   #:channel
   #:defchannel
   #:->
   #:<-
   #:receive))

(defpackage fibers
  (:use :common-lisp :utilities :channels)
  (:export 
   #:channel
   #:defchannel
   #:scheduler
   #:create-scheduler
   #:spush
   #:spop
   #:stack
   #:->
   #:-<
   #:*->
   #:<-*
   #:spawn
   #:yield
   #:peek
   #:next
   #:bind
   #:jump
   #:jump-when
   #:call
   #:exit-proc
   #:run
   #:priority-run
   #:receive
   #:dmac
   #:define
   #:flambda))


(defpackage cost-calc
  (:use :common-lisp :utilities :channels)
  (:export
   #:INFINITY
   #:EPSILON
   #:MAX-DEPTH
   #:DEPTH
   #:$STACK
   #:COST
   #:SETCOST
   #:DEFCOST
   #:SETEPS
   #:DEFEPS
   #:DEF$
   #:$MIN
   #:$MAX
   #:$SEQ))


