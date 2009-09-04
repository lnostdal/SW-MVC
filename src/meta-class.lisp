;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass mvc-class (standard-class)
  ()

  (:documentation "
This metaclass enables slot access to have CELL-like features.

Initialization of unbound slots has one special property; if initialized with a
CELL instance, that CELL will be used to directly represent the slot value.

To initialize a slot with a CELL as a value use AMX:MK-PTR or the #& reader
macro. If a pointer is to be stored as a value, just wrap it in another
pointer.

The CELL-OF macro allows one to refer directly to a CELL used to represent a
slot:

    (cell-of (slot-value some-object 'some-slot)) => #<CELL ...>
    (setf (cell-of (slot-value some-object 'some-slot)) :replace-the-cell-itself)

This will also work for accessor methods (i.e., not just SLOT-VALUE)."))


(defmethod validate-superclass ((class mvc-class) (superclass standard-class))
  t)


(defmethod validate-superclass ((class mvc-class) (superclass stm-class))
  #| Because doing this will cause access to slots in the superclass to change behavior when done "from" an instance
  of the subclass. |#
  nil)


(defmethod value-supplied-p ((class stm-class) (superclass mvc-class))
  #| I'm not sure what would happen here, so we disallow it for now. |#
  nil)



(defclass direct-cell-slotd (standard-direct-slot-definition)
  ((cellp :reader cellp-of :initarg :cellp
          :initform t)))


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


(defmethod cellp-of ((slotd effective-cell-slotd))
  (cellp-of (direct-slotd-of slotd)))


(defmethod compute-effective-slot-definition ((class mvc-class) name direct-slot-definitions)
  (declare (ignore name)
           (list direct-slot-definitions))
  (flet ((is-cell-slot (slot) (typep slot 'direct-cell-slotd)))
    (declare (inline is-cell-slot))
    (let ((*direct-cell-slotd* (find-if #'is-cell-slot direct-slot-definitions)))
      (call-next-method))))


(defmethod effective-slot-definition-class ((class mvc-class) &rest initargs)
  (declare (ignore initargs))
  (if *direct-cell-slotd*
      (find-class 'effective-cell-slotd)
      (call-next-method)))



(defmethod slot-value-using-class ((class mvc-class) instance (slotd effective-cell-slotd))
  (if (cellp-of slotd)
      (let* ((get-cell-p *get-cell-p*)
             (*get-cell-p* nil)
             (value (call-next-method)))
        (cond
          ((and (typep value 'cell) (not get-cell-p))
           (cell-deref value))

          (t
           value)))
      (call-next-method)))


(defmethod (setf slot-value-using-class) (new-value (class mvc-class) instance (slotd effective-cell-slotd))
  (if (cellp-of slotd)
      (let ((get-cell-p *get-cell-p*)
            (*get-cell-p* nil))
        (if-let ((cell (when (and (not get-cell-p) (slot-boundp-using-class class instance slotd))
                         ;; Will not call CELL-DEREF, and thus will not cause new dependencies to be added.
                         (cell-of (slot-value-using-class class instance slotd) :errorp t))))
          (setf (cell-deref cell) new-value)
          (call-next-method (typecase new-value
                              (pointer (let ((inner-value (ptr-value new-value)))
                                         (typecase inner-value
                                           (cell inner-value) ;; "Formula".
                                           (t (mk-icell inner-value)))))
                              (t (mk-icell new-value)))
                            class instance slotd)))
      (call-next-method)))
