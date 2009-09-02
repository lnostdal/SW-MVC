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

This will also work for accessor methods (i.e., not just SLOT-VALUE)."))


(defmethod validate-superclass ((class mvc-class) (superclass standard-class))
  t)


(defmethod slot-value-using-class ((class mvc-class) instance slotd)
  (let* ((get-cell-p *get-cell-p*)
         (*get-cell-p* nil)
         (value (call-next-method)))
    (cond
      ((and (typep value 'cell) (not get-cell-p))
       (cell-deref value))

      (t
       value))))


(defmethod (setf slot-value-using-class) (new-value (class mvc-class) instance slotd)
  (let ((get-cell-p *get-cell-p*)
        (*get-cell-p* nil))
    (if-let ((cell (when (and (not get-cell-p)
                              (slot-boundp-using-class class instance slotd))
                     (withp (cell-of (slot-value-using-class class instance slotd)) ;; Will not call CELL-DEREF.
                       (typep it 'cell)))))
      (setf (cell-deref cell) new-value)
      (call-next-method))))
