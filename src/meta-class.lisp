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
    (setf (cell-of (slot-value some-object 'some-slot)) :replace-the-cell-itself)

This will also work for accessor methods (i.e., not just SLOT-VALUE).

  Supplying :CELLP T as a slot option will cause the slot to _always_ have
a CELL backend storage. Using AS-VALUE allows one to initialize and assign such
a slot with a CELL as a value instead of treating that CELL as a new CELL
representing the slot.
E.g.,

  SW-MVC> (let ((some-cell #λ42))
            (defclass some-class ()
              ((some-slot :cellp t :initform (as-value some-cell)))
              (:metaclass mvc-class))
            (assert (eq some-cell (slot-value (make-instance 'some-class) 'some-slot))))
  NIL"))


(defmethod validate-superclass ((class mvc-class) (superclass standard-class))
  t)



;;; :CELLP keyarg support for DEFCLASS forms follows.

(defclass direct-cell-slotd (standard-direct-slot-definition)
  ((cellp :reader cellp-of :initarg :cellp)))


(defmethod direct-slot-definition-class ((class mvc-class) &key
                                         (cellp t)
                                         (type nil type-supplied-p)
                                         &allow-other-keys)
  (assert (not type-supplied-p) nil
          "Type checking etc. not implemented for MVC-CLASS classes yet. Got:~%~S" type)
  (if cellp
      (find-class 'direct-cell-slotd)
      (call-next-method)))


(define-variable *direct-cell-slotd*
    :value nil)


(defclass effective-cell-slotd (standard-effective-slot-definition)
  ((direct-slotd :initform *direct-cell-slotd* :reader direct-slotd-of)))


(defmethod compute-effective-slot-definition ((class mvc-class) name direct-slot-definitions)
  (declare (ignore name)
           (list direct-slot-definitions))
  (flet ((is-cell (slot) (typep slot 'direct-cell-slotd)))
    (declare (inline is-cell))
    (let ((*direct-cell-slotd* (find-if #'is-cell direct-slot-definitions)))
      (call-next-method))))


(defmethod effective-slot-definition-class ((class mvc-class) &rest initargs)
  (declare (ignore initargs))
  (if *direct-cell-slotd*
      (find-class 'effective-cell-slotd)
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
  (let ((get-cell-p *get-cell-p*)
        (*get-cell-p* nil))
    (if-let ((old-value (and (slot-boundp-using-class class instance slotd)
                             (cell-of (slot-value-using-class class instance slotd)))))
      (cond
        ;; TODO: This deals with CELL only; what about SINGLE-VALUE-MODEL (and perhaps even MODEL)?
        ((and (typep old-value 'cell) (not get-cell-p))
         (setf (cell-deref old-value) new-value))

        (t
         (call-next-method)))

      ;; Slot was not bound..
      (if (and (typep slotd 'effective-cell-slotd)
               ;; ..(this'll enable us to store CELL instances in slots of this metaclass)..
               (not (typep new-value 'cell)))
          ;; ..and it should always be a CELL as requested by the :CELLP slot keyarg.
          (call-next-method (make-instance 'cell
                                           :formula (if (and (consp new-value)
                                                             (eq '%as-value (car new-value)))
                                                        (lambda () (cdr new-value))
                                                        (lambda () new-value))
                                           :input-evalp t
                                           :output-evalp nil)
                            class instance slotd)
          (call-next-method)))))


(declaim (inline as-value))
(defun as-value (arg)
  "This enables one to initialize a CLOS \"CELL-slot\" with a CELL as a value."
  (cons '%as-value arg))