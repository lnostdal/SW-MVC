;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(defclass container (#|model-base|#)
  ((equality-test :accessor equality-test-of
                  :initform #'eq
                  :documentation "
This might be used to determine whether an element that is to be added to the
container in question already exists in the container."))

  (:metaclass mvc-stm-class))


(defmethod container-of ((container container))
  container)
