;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(defclass container (self-ref multiple-value-model) ;; TODO: Think about use of SELF-REF here..
  ((test-fn :accessor test-fn-of :initarg :test-fn
            :type function
            :initform #'eql)

   (key-fn :accessor key-fn-of :initarg :key-fn
           :type function
           :initform #'identity))

  (:metaclass mvc-class))


(defmethod container-of ((container container))
  container)


(defmethod container-find ((value model) (container container))
  "Find VALUE in CONTAINER."
  (node-of value))