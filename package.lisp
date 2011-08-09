(defpackage "COST-CALC"
  (:use "COMMON-LISP")
  (:export
   "INFINITY"
   "EPSILON"
   "MAX-DEPTH"
   "DEPTH"
   "$STACK"
   "COST"
   "SETCOST"
   "DEFCOST"
   "SETEPS"
   "DEFEPS"
   "DEF$"
   "$MIN"
   "$MAX"
   "$SEQ"))

(defpackage :fibers
  (:use :common-lisp)
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