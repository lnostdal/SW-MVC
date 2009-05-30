;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)


(defclass container ()
  ()
  
  (:metaclass mvc-stm-class))


(defmethod container-of ((container container))
  container)
