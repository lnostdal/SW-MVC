;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass mvc-class (standard-class)
  ()
  (:documentation "
  This metaclass enables slot access to have CELL-like features.

  The CELL-OF macro allows one to refer directly to a CELL used to represent a
slot:

    (cell-of (slot-value some-object 'some-slot))
    (setf (cell-of (slot-value some-object 'some-slot)) :replace-the-cell)

This will also work for accessor methods (i.e., not just SLOT-VALUE).

  Supplying :CELLP T as a slot option will cause the slot to _always_ have
a CELL backend storage. Using AS-VALUE allows one to initialize such a slot
with a function as a value instead of treating it as a formula yielding a value.
E.g.,

  SW-MVC> (let ((some-function (lambda () 42)))
            (defclass some-class ()
              ((some-slot :cellp t :initform (as-value some-function)))
              (:metaclass mvc-class))
            (assert (eq some-function (slot-value (make-instance 'some-class) 'some-slot))))
  NIL"))


(defmethod validate-superclass ((class mvc-class) (superclass standard-class))
  t)



;;; :CELLP keyarg support for DEFCLASS forms follows.

(defclass direct-cell-slot (standard-direct-slot-definition)
  ((cellp :reader cellp-of :initarg :cellp
          :initform nil)))

(defmethod direct-slot-definition-class ((class mvc-class) &key cellp &allow-other-keys)
  (if cellp
      (find-class 'direct-cell-slot)
      (call-next-method)))

(define-variable *direct-cell-slot*
    :value nil)


(defclass effective-cell-slot (standard-effective-slot-definition)
  ((direct-slotd :initform *direct-cell-slot* :reader direct-slot-of)))


(defmethod compute-effective-slot-definition ((class mvc-class) name direct-slot-definitions)
  (declare (ignore name))
  (flet ((is-cell (slot) (typep slot 'direct-cell-slot)))
    (let ((*direct-cell-slot* (find-if #'is-cell direct-slot-definitions)))
      (call-next-method))))


(defmethod effective-slot-definition-class ((class mvc-class) &rest initargs)
  (declare (ignore initargs))
  (if *direct-cell-slot*
      (find-class 'effective-cell-slot)
      (call-next-method)))



;;; Reader and writer for MVC-CLASS.

(defmethod slot-value-using-class :around ((class mvc-class) instance slotd)
  (let ((value (call-next-method)))
    (cond
      ;; TODO: This deals with CELL only; what about SINGLE-VALUE-MODEL (and perhaps even MODEL)?
      ((and (typep value 'cell) (not *get-cell-p*))
       (cell-deref value))

      (t
       value))))


(defmethod (setf slot-value-using-class) :around (new-value (class mvc-class) instance slotd)
  (if-let ((old-value (and (slot-boundp-using-class class instance slotd)
                           (cell-of (slot-value-using-class class instance slotd)))))
    (cond
      ;; TODO: This deals with CELL only; what about SINGLE-VALUE-MODEL (and perhaps even MODEL)?
      ((and (typep old-value 'cell) (not *get-cell-p*))
       (setf (cell-deref old-value) new-value))

      (t
       (let ((*get-cell-p* nil))
         (call-next-method))))
    ;; Slot was not bound..
    (let ((*get-cell-p* nil))
      (if (typep slotd 'effective-cell-slot)
          ;; ..and it should always be a CELL as requested by the :CELLP slot keyarg.
          (call-next-method (make-instance 'cell
                                           :formula
                                           (if (functionp new-value)
                                               new-value
                                               ;; It should be possible to store functions as values in CELLs.
                                               (if (and (consp new-value)
                                                        (eq '%as-value (car new-value)))
                                                   (lambda () (cdr new-value))
                                                   (lambda () new-value)))
                                           :input-evalp t
                                           :output-evalp nil)
                            class instance slotd)
          (call-next-method)))))


(declaim (inline as-value))
(defun as-value (function)
  "This enables one to initialize a CLOS CELL-slot with a FUNCTION as a value."
  ;;(declare (function function))
  (cons '%as-value function))