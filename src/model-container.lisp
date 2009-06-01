;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(defclass container (self-ref)
  ((test-fn :accessor test-fn-of :initarg :test-fn
            :type function
            :initform #'eql)

   (key-fn :accessor key-fn-of :initarg :key-fn
           :type function
           :initform #'identity))
  
  (:metaclass mvc-stm-class))


(defmethod container-of ((container container))
  container)


(defmethod container-find (value (container container))
  (find value ~container
        :test (test-fn-of container)
        :key (key-fn-of container)))
