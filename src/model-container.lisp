;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass container (self-ref multiple-value-model)
  ((test-fn :accessor test-fn-of :initarg :test-fn
            :type function
            :initform #'eql)

   (key-fn :accessor key-fn-of :initarg :key-fn
           :type function
           :initform #'identity))

  (:metaclass mvc-class))


(defmethod container-of ((container container))
  container)


(defmethod container-find (value (container container))
  "Find VALUE in CONTAINER."
  (find value ~container
        :test (test-fn-of container)
        :key (key-fn-of container)))
