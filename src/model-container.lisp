;;;; http://nostdal.org/ ;;;;

(in-package :sw-mvc)
=common-headers=


(defclass container (multiple-value-model)
  ()

  (:metaclass mvc-class))


(defgeneric empty-p-of (container))
