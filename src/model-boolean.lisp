;;;; http://nostdal.org/ ;;;;

(in-package :sw-mvc)
=common-headers=


(defclass boolean-model (single-value-model)
  ((value :accessor value-of :initarg :value
          :initform nil)

   (old-t :reader old-t-of
          :initform t))

  (:metaclass mvc-class)
  (:documentation "
Boolean state thingy with \"memory\" of T state value."))


(add-deref-type 'boolean-model
                :get-expansion (λ (arg-sym) `(value-of ,arg-sym))
                :set-expansion t)


(defmethod (setf value-of) :before ((new-value (eql nil)) (boolean-model boolean-model))
  (with-object boolean-model
    (setf ¤old-t ¤value)))


(defmethod toggle ((boolean-model boolean-model))
  (with-object boolean-model
    (if ¤value
        (nilf ¤value)
        (setf ¤value ¤old-t))))
