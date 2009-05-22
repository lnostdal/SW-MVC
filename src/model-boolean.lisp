;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass boolean-model (model-base single-value-model)
  ((value :accessor value-of :initarg :value
          :initform nil)

   (old-t :reader old-t-of
          :initform t))

  (:metaclass mvc-stm-class)
  (:documentation "
Boolean state thingy with \"memory\" of T state value."))


(defmethod deref ((boolean-model boolean-model))
  (value-of boolean-model))


(defmethod (setf deref) (new-state (boolean-model boolean-model))
  (setf (value-of boolean-model) new-state))


(defmethod (setf value-of) :before ((new-value (eql nil)) (boolean-model boolean-model))
  (with-object boolean-model
    (setf ¤old-t ¤value)))


(defmethod toggle ((boolean-model boolean-model))
  (with-object boolean-model
    (if ¤value
        (nilf ¤value)
        (setf ¤value ¤old-t))))

