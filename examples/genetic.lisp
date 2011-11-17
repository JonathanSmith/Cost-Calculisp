(in-package :cost-calc)

(defclass instruction ($node)
  ((fun :accessor fun)
   (arity :accessor arity)
   (args :accessor args)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *instructions* nil)
  (defmacro define-instruction (name function-symbol arity)
    (push name *instructions*)
    `(defclass ,name (instruction)
       ((fun :accessor fun :initform ',function-symbol)
	(arity :accessor arity :initform ,arity)))))

(defclass value ($node)
  ((value :accessor value)))


(define-instruction plus + 2)
(define-instruction minus - 2)
(define-instruction div / 2)
(define-instruction times * 2)
(define-instruction land logand 2)
(define-instruction lior logior 2)
(define-instruction lxor logxor 2)
(define-instruction lnot lognot 1)
(define-instruction lsqrt sqrt 1)
(define-instruction lash ash 2)

(defmacro make-value (integer)
  (let ((i (gensym)))
    `(let ((,i ,integer))
       (make-instance 'value :value ,i))))


(defun make-instruction (name)
  (make-instance name))

(defun make-instruction-tree (val)
  (let ((instruction (make-instruction (elt *instructions* (random (length *instructions*))))))
    (dotimes (i (arity instruction))
      (push (make-value val) (args instruction)))
    instruction))
  