;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(class-forward-reference view-base)

#|(class-forward-reference model-base
  (:metaclass mvc-stm-class))|#