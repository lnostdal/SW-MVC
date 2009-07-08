;;;; http://nostdal.org/ ;;;;

(in-package #:sw-mvc)

(declaim #.(optimizations))


(class-forward-reference view-base)
(class-forward-reference cell
  (:metaclass stm-class))
