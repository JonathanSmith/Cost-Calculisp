(in-package :cost-calc)

(defvar *register-count* 4)
(defvar *register-type* 'number)

(defclass formula ($node)
  ((insts :accessor insts :initarg :insts)
   (registers :accessor registers :initarg :registers)
   (args :accessor args :initarg :args)))

(defclass register ()
  ((initval :accessor initval :initarg :initval)
   (id :accessor id :initarg :id)))

(defun make-random-register (id)
  (make-instance 'register :initval (random most-positive-fixnum) :id id))

;;question of how best to chain instructions
;;is interesting. we could do a push/pop stack,
;;or we can do an ast, or we could do something like
;;a register machine. if we do a register machine, 
;;our instruction representation can be linear, which
;;should make it easier to permute.
(defmethod generate-formula-code ((formula formula))
  (let* ((instructions (insts formula))
	 (args (args formula))
	 (registers (registers formula))
	 (gregisters (gensym)))
    `(lambda (,@args)
       (let ((,gregisters (make-array ,*register-count* :element-type ',*register-type*)))
	 ,@(loop for arg in args
	      for i from 0 by 1 collect `(setf (elt ,gregisters ,i) ,arg))
	 ,@(loop 
	      for register in registers
	      for i from (length args) below *register-count*
	      collect `(setf (elt ,gregisters ,i) ,(initval register)))

	 ,@(loop for inst in instructions collect
		(generate-code inst gregisters))
	 (elt ,gregisters 0)))))

(defclass instruction ()
  ((fun :accessor fun)
   (arity :accessor arity)
   (to :accessor to :initarg :to)
   (from :accessor from :initarg :from)))

(defmethod generate-code ((instruction instruction) registers)
  (let* ((target `(elt ,registers ,(to instruction)))
	 (used-registers (mapcar (lambda (reg-id) `(elt ,registers ,reg-id)) (from instruction))))
    `(setf ,target (,(fun instruction) ,@used-registers))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *instructions* nil)
  (defmacro define-instruction (name function-symbol arity)
    (push name *instructions*)
    `(defclass ,name (instruction)
       ((fun :accessor fun :initform ',function-symbol)
	(arity :accessor arity :initform ,arity)))))

(defun div (a b)
  (cond ((= a 0) a)
	((= b 0) most-positive-fixnum)
	(t (truncate a b))))


(define-instruction plus + 2)
(define-instruction minus - 2)
(define-instruction div div 2)
(define-instruction times * 2)
(define-instruction nop identity 0)
(define-instruction copy identity 1)

(defmethod generate-code ((nop nop) registers)
  (declare (ignore registers))
  `(progn))


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

(defun make-random-instruction ()
  (let ((inst (make-instance (elt *instructions* (random (length *instructions*)))
			     :to (random *register-count*))))
    (setf (from inst) (loop for i from 0 below (arity inst) collect (random *register-count*)))
    inst))

(defun make-random-instructions (count)
  (loop for i from 0 below count collect
       (make-random-instruction)))
 
(defun make-random-formula (&rest args)
  (make-instance 'formula
		 :insts (make-random-instructions (random 10)) 
		 :args args
		 :registers (loop for i from (length args) below *register-count* collect (make-random-register i))))

(defun random-lambda (&rest args)
  (generate-formula-code (apply #'make-random-formula args)))

(defun square (a)
  (* a a))

(defun cube (a)
  (* a a a))

(defun stupid (a)
  (/ (+ a a) a))

(defvar *input-set* (loop for i from 1 to 20 collect i))

(defparameter *expectations* (mapcar #'stupid *input-set*))

(defun chi-squared (observations expectations)
  (reduce #'+ (mapcar (lambda (observed expected)
			(/ (square (- observed expected)) expected))
		      observations expectations)))

(defun comparator (lambda)
  (chi-squared (mapcar lambda *input-set*) *expectations*))

(defun random-comp ()
  (let ((code (random-lambda 'a)))
    (cons (comparator (eval code)) code)))

;;(loop for (a . b) =  (random-comp) until (eql a 0) finally (return (values a b)))
;;....the stupid way.

(defun crossover (formula1 formula2)
  ((random (length (insts formula1)))
  