;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass mvc-class (standard-class)
  ()
  (:documentation "
This metaclass enables transparent access to CELLs's values stored in CLASS
slots.

The CELL-OF macro allows one to refer to the CELL stored in the slot:

  (cell-of (slot-value some-object 'some-slot))
  (setf (cell-of (slot-value some-object 'some-slot)) :replace-the-cell)

This will also work for accessor methods."))


(defmethod validate-superclass ((class mvc-class) (superclass standard-class))
  t)


(defclass direct-cell-slot (standard-direct-slot-definition)
  ((cell-p :reader cell-p-of :initarg :cell-p
           :initform nil)))

(defmethod direct-slot-definition-class ((class mvc-class) &key (cell-p nil cell-p-supplied-p) &allow-other-keys)
  (declare (ignore cell-p-supplied-p))
  (if cell-p
      (find-class 'direct-cell-slot)
      (call-next-method)))

(defvar *direct-cell-slot* nil)

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


(defmethod slot-value-using-class :around ((class mvc-class) instance slotd)
  (let ((value (call-next-method)))
    (cond
      ((and (typep value 'cell) (not *get-cell-p*))
       ~value)

      (t
       value))))


(defmethod (setf slot-value-using-class) :around (new-value (class mvc-class) instance slotd)
  (if-let ((old-value (and (slot-boundp-using-class class instance slotd)
                           (cell-of (slot-value-using-class class instance slotd)))))
    (cond
      ((and (typep old-value 'cell) (not *get-cell-p*))
       (setf ~old-value new-value))

      (t
       (let ((*get-cell-p* nil))
         (call-next-method))))
    (let ((*get-cell-p* nil))
      (if (typep slotd 'effective-cell-slot)
          (call-next-method (if (functionp new-value)
                                (make-instance 'cell
                                               :formula new-value
                                               :input-evalp t
                                               :output-evalp nil)
                                λinew-value)
                            class instance slotd)
          (call-next-method)))))
